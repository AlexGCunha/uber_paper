################################################################################
#This code will:
#- Read RAIS files and create a quarterly panel of employment
################################################################################
library(tidyverse)
library(data.table)
library(arrow)
library(readxl)

rais_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/RAIS/"

years = 2012:2021
precos = read_excel("../data/deflator_inpc.xlsx", sheet = "anual_junho")
cbo = read_parquet("../data/cbo_3digs.parquet")
setDT(precos)
precos[, indice :=NULL]

rais_agg = data.table()
for(y in years){
  #Abrir arquivo para esse estado/ano
  filename = paste0(rais_path, "rais_",y,".parquet")
  rais = read_parquet(filename) %>% data.table()
  rais[, natureza_juridica := as.integer(natureza_juridica)]
  
  #corrigir salarios para inflacao (usar deflator de junho de cada ano)
  rais = merge(rais, precos, by = "ano", all.x = TRUE)
  rais[, salario := valor_remuneracao_media * deflator_24]
  
  #adicionar salario medio e rank por cbo
  rais[, cbo_3dig := substr(rais$cbo_2002, 1, 3)]
  rais = merge(rais, cbo, by = 'cbo_3dig', all.x = TRUE)
  
  #adicionar grupos de idade
  rais[, grupo_idade := fcase(idade <= 30, 1,
                              idade > 30 & idade < 45, 2,
                              idade > 45, 3, default = NA)]
  
  #print share of workers by cbo status
  if(y == 2014){
    teste = count(rais, rank_wage_cbo) %>% 
      mutate(share = n/sum(n)) %>% 
      print()
  }
  
  ##indicadores de tipo de emprego
  vinculos_publicos = c(30, 31, 35)
  vinculos_rurais = c(20, 25, 70, 75)
  #privado full time sem prazo determinado
  rais[, privado := fcase(!(tipo_vinculo %in% vinculos_publicos) & 
                            !(tipo_vinculo %in% vinculos_rurais) &
                            natureza_juridica >2038, 1, default =0)]
  
  #publico
  rais[, publico := fcase(tipo_vinculo %in% vinculos_publicos, 1, default = 0)]
  
  #privado temporario
  rais[,temporario := fcase(tipo_vinculo %in% c(50, 60, 90, 95, 96, 97) &
                              natureza_juridica > 2038, 1, default = 0)]
  
  
  #privado meio período
  rais[, meio_periodo := fcase(!(tipo_vinculo %in% vinculos_publicos) &
                                 !(tipo_vinculo %in% vinculos_rurais) &
                                 quantidade_horas_contratadas <= 20 &
                                 natureza_juridica > 2038, 1, default = 0)]
  
  #rural
  rais[, rural := fcase(!(tipo_vinculo %in% vinculos_publicos) & 
                          tipo_vinculo %in% vinculos_rurais &
                          natureza_juridica >2038, 1, default =0)]
  
  #rodar loop para definir variaveis ao nivel trimestral:
  for(s in 1:2){
    ##filtrar pessoas trabalhando nesse trimestre
    mes_aux = s*6
    min_mes_semestre = ifelse(s == 1, 1, 7)
    
    #definir admitidos e demitidos
    rais_alt = copy(rais)
    rais_alt[, `:=`(
      demitido_privado = fcase(mes_desligamento <= mes_aux
                               & mes_desligamento >= min_mes_semestre
                               & privado == 1, 1, default = 0),
      admitido_privado = fcase(mes_admissao <= mes_aux
                               & mes_admissao >= min_mes_semestre
                               & privado == 1, 1, default = 0))] 
    rais_alt = rais_alt[, .(demitido = sum(demitido_privado),
                            admitido = sum(admitido_privado)),
                        by = .(id_municipio)]
    
    #dropar ainda não foi admitidos
    rais_s = rais[mes_admissao <= mes_aux | is.na(mes_admissao)]
    #dropar já demitidos
    rais_s = rais_s[mes_desligamento > mes_aux | is.na(mes_desligamento)]
    
    #corrigir tempo de emprego para o primeiro semestre
    if(s == 1){
      rais_s[, mes_demissao_aux := fcase(is.na(mes_desligamento), 
                                         as.integer(12), default = mes_desligamento)]
      rais_s[, tempo_emprego := tempo_emprego - (mes_demissao_aux - 6)]
      
      aux = min(rais_s$tempo_emprego)
      if(aux < 0){
        print(paste0("Minimo tenure arredondado para 0:", aux))
        rais_s[, tempo_emprego := max(0, tempo_emprego)]
      }
    }
    
    
    
    ##Agrupar por tipo de emprego
    privado = rais_s[privado == 1, 
                     .(emprego_privado = .N
                       ,tenure_privado = mean(tempo_emprego)
                       ,salario_privado = mean(salario)
                     ),
                     by = .(id_municipio)]
    
    homens = rais_s[privado == 1 & sexo == "1", 
                    .(emprego_homens= .N
                      ,tenure_homens = mean(tempo_emprego)
                      ,salario_homens = mean(salario)
                    ),
                    by = .(id_municipio)]
    
    mulheres = rais_s[privado == 1 & sexo == "2", 
                      .(emprego_mulheres = .N
                        ,tenure_mulheres = mean(tempo_emprego)
                        ,salario_mulheres = mean(salario)
                      ),
                      by = .(id_municipio)]
    
    privado_lths = rais_s[privado == 1 & grau_instrucao_apos_2005 < 7, 
                          .(emprego_lths = .N 
                            ,tenure_lths = mean(tempo_emprego) 
                            ,salario_lths = mean(salario)
                          ),
                          by = .(id_municipio)]
    
    privado_hs_somecol = rais_s[privado == 1 & grau_instrucao_apos_2005  %in% c(7,8), 
                                .(emprego_hs_somecol = .N
                                  ,tenure_hs_somecol = mean(tempo_emprego) 
                                  ,salario_hs_somecol = mean(salario)
                                ),
                                by = .(id_municipio)]
    
    privado_col = rais_s[privado == 1 & grau_instrucao_apos_2005  >8, 
                         .(emprego_col = .N
                           ,tenure_col = mean(tempo_emprego) 
                           ,salario_col = mean(salario)
                         ),
                         by = .(id_municipio)]
    
    privado_baixo_sal = rais_s[privado == 1 & salario <= 1500,
                               .(emprego_baixo_sal = .N,
                                 tenure_baixo_sal = mean(tempo_emprego)),
                               by = .(id_municipio)]
    
    privado_med_sal = rais_s[privado == 1 & salario > 1500 & salario <= 3000,
                             .(emprego_med_sal = .N,
                               tenure_med_sal = mean(tempo_emprego)),
                             by = .(id_municipio)]
    
    privado_alto_sal = rais_s[privado == 1 & salario > 3000 & salario <= 6000,
                              .(emprego_alto_sal = .N,
                                tenure_alto_sal = mean(tempo_emprego)),
                              by = .(id_municipio)]
    
    privado_altissimo_sal = rais_s[privado == 1 & salario > 6000,
                                   .(emprego_altissimo_sal = .N,
                                     tenure_altissimo_sal = mean(tempo_emprego)),
                                   by = .(id_municipio)]
    
    baixo_cbo = rais_s[privado == 1 & rank_wage_cbo == 4,
                       .(emprego_baixo_cbo = .N,
                         salario_baixo_cbo = mean(salario)),
                       by = .(id_municipio)]
    med_cbo = rais_s[privado == 1 & rank_wage_cbo == 3,
                     .(emprego_med_cbo = .N,
                       salario_med_cbo = mean(salario)),
                     by = .(id_municipio)]
    alto_cbo = rais_s[privado == 1 & rank_wage_cbo == 2,
                      .(emprego_alto_cbo = .N,
                        salario_alto_cbo = mean(salario)),
                      by = .(id_municipio)]
    
    
    altissimo_cbo = rais_s[privado == 1 & rank_wage_cbo == 1,
                           .(emprego_altissimo_cbo = .N,
                             salario_altissimo_cbo = mean(salario)),
                           by = .(id_municipio)]
    
    idade_baixo = rais_s[privado == 1 & grupo_idade == 1,
                         .(emprego_baixo_idade = .N,
                           salario_baixo_idade = mean(salario)),
                         by = .(id_municipio)]
    
    idade_med= rais_s[privado == 1 & grupo_idade == 2,
                         .(emprego_med_idade = .N,
                           salario_med_idade = mean(salario)),
                         by = .(id_municipio)]
    
    idade_alto = rais_s[privado == 1 & grupo_idade == 3,
                         .(emprego_alto_idade = .N,
                           salario_alto_idade = mean(salario)),
                         by = .(id_municipio)]
    
    
    publico = rais_s[publico == 1, 
                     .(emprego_publico = .N,
                       tenure_publico = mean(tempo_emprego),
                       salario_publico = mean(salario)),
                     by =.(id_municipio)]
    
    temporario = rais_s[temporario == 1,
                        .(emprego_temporario = .N,
                          tenure_temporario = mean(tempo_emprego),
                          salario_temporario = mean(salario)),
                        by = .(id_municipio)]
    
    meio_periodo = rais_s[meio_periodo == 1,
                          .(emprego_meio_periodo = .N,
                            tenure_meio_periodo = mean(tempo_emprego),
                            salario_meio_periodo = mean(salario)),
                          by = .(id_municipio)]
    
    
    
    rural = rais_s[rural == 1,
                   .(emprego_rural = .N,
                     tenure_rural = mean(tempo_emprego),
                     salario_rural = mean(salario)),
                   by = .(id_municipio)]
    
    
    #Juntar informações
    municipios = data.table(id_municipio = unique(rais$id_municipio))
    combinado = merge(municipios, privado,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, rais_alt,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, homens,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, mulheres,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, privado_lths,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, privado_hs_somecol,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, privado_col,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, privado_baixo_sal,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, privado_med_sal,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, privado_alto_sal,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, privado_altissimo_sal,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, baixo_cbo,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, med_cbo,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, alto_cbo,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, altissimo_cbo,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, idade_baixo,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, idade_med,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, idade_alto,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, publico,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, temporario,
                      by = "id_municipio", all.x = TRUE)
    
    
    combinado = merge(combinado, meio_periodo,
                      by = "id_municipio", all.x = TRUE)
    
    
    combinado = merge(combinado, rural,
                      by = "id_municipio", all.x = TRUE)
    
    
    combinado[, anosem := as.numeric(paste0(y,s))]
    
    #juntar no rais_agg
    rais_agg = rbind(rais_agg, combinado)
    
    rm(combinado, municipios, privado,  publico,
       temporario, rais_s, meio_periodo)
  }
  rm(rais)
  print(y)
  gc()
}

#garantir uma unica observacao por municipio por periodo
rais_agg[, count := seq_len(.N), by = c("id_municipio", "anosem")]
rais_agg = rais_agg[count == 1]
rais_agg[, count := NULL]

#trocar ordem das colunas para alfabetica
nova_ordem = sort(colnames(rais_agg))
rais_agg = rais_agg[, ..nova_ordem]
rm(nova_ordem)

####Criar painel balanceado
bal_panel = expand_grid(
  id_municipio = unique(rais_agg$id_municipio),
  anosem = unique(rais_agg$anosem)) %>%
  mutate(ano = as.integer(substr(anosem, 1,4))) %>% 
  data.table()

bal_panel = merge(bal_panel, rais_agg,
                  by = c("id_municipio", "anosem"),
                  all.x = TRUE)
rm(rais_agg)

#Inputar emprego 0 para os locais sem emprego (NA)
cols_emprego = bal_panel %>% select(starts_with("emprego")) %>% colnames()
bal_panel[, (cols_emprego) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)),
          .SDcols = cols_emprego]

#Salvar
write_parquet(bal_panel, "../data/ub_painel_emprego_basico.parquet")
rm(list = ls())
gc()
