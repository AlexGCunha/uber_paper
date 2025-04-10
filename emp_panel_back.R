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
  rais[, valor_remuneracao_media := valor_remuneracao_media * deflator_10]
  
  #base com dados de salários por ocupação
  # salario_cbo = read_parquet("../data/cbo_3digs.parquet")
  
  #adicionar salário com base nos 3 primeiros dígitos da ocupação (CBO)
  # rais[, cbo_3dig := substr(cbo_2002, 1,3)]
  # rais = merge(rais, salario_cbo, by = "cbo_3dig", all.x = TRUE)
  
  #mudanca: antes considerava emprego privado (e meio periodo) como tipo_vinculo == 10.
  #agora estou considerando todos os empregos
  vinculos_publicos = c(30, 31, 35)
  vinculos_rurais = c(20, 25, 70, 75)
  
  
  ##indicadores de tipo de emprego
  #privado full time sem prazo determinado
  rais[, privado := fcase(!(tipo_vinculo %in% vinculos_publicos) & 
                            !(tipo_vinculo %in% vinculos_rurais) &
                            natureza_juridica >2038, 1, default =0)]
  
  ##tipo_vinculo = 10:  Trabalhador urbano vinculado a empregador pessoa jurídica
  # por contrato de trabalho regido pela CLT, por prazo indeterminado
  
  #publico
  rais[, publico := fcase(tipo_vinculo %in% vinculos_publicos, 1, default = 0)]
  
  #privado temporario
  rais[,temporario := fcase(tipo_vinculo %in% c(50, 60, 90, 95, 96, 97) &
                              natureza_juridica > 2038, 1, default = 0)]
  
  rais[, n_temporario := fcase(!(tipo_vinculo %in% vinculos_publicos) & 
                                 !(tipo_vinculo %in% vinculos_rurais) &
                                 !(tipo_vinculo %in% c(50, 60, 90, 95, 96, 97)) &
                                 natureza_juridica >2038, 1, default =0)]
  
  #privado meio período
  rais[, meio_periodo := fcase(!(tipo_vinculo %in% vinculos_publicos) &
                                 !(tipo_vinculo %in% vinculos_rurais) &
                                 quantidade_horas_contratadas <= 20 &
                                 natureza_juridica > 2038, 1, default = 0)]
  
  #privado, urbano, por prazo indeterminado
  rais[, privado_full := fcase(tipo_vinculo %in% c(10) & 
                                 !(tipo_vinculo %in% vinculos_rurais) &
                                 natureza_juridica > 2038, 1, default = 0)]
  
  #rural
  rais[, rural := fcase(!(tipo_vinculo %in% vinculos_publicos) & 
                          tipo_vinculo %in% vinculos_rurais &
                          natureza_juridica >2038, 1, default =0)]
  
  #rodar loop para definir variaveis ao nivel trimestral:
  for(s in 1:2){
    ##filtrar pessoas trabalhando nesse trimestre
    mes_aux = s*6
    #definir empregados como todos, com excessão de:
    #-ainda não admitidos
    #-já demitidos
    rais_s = copy(rais)
    rais_s[, empregado := fcase(
      mes_admissao > mes_aux & !is.na(mes_admissao), 0,
      mes_desligamento <= mes_aux & !is.na(mes_desligamento), 0,
      default = 1
    )]
    
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
    
    #definir admitidos e demitidos no trimestre
    if(s == 1){
      rais_s[, `:=`(admitido = fcase(mes_admissao <= 6, 1, default = 0),
                    demitido = fcase(mes_desligamento <= 6
                                     & motivo_desligamento == '11', 
                                     1, default = 0),
                    demitido_total = fcase(mes_desligamento <= 6,
                                           1, default = 0)
      )]
    } else{
      rais_s[, `:=`(admitido = fcase(mes_admissao %in% c(7:12), 1, default = 0),
                    demitido = fcase(mes_desligamento %in% c(7:12)
                                     & motivo_desligamento == '11',
                                     1, default = 0),
                    demitido_total = fcase(mes_desligamento %in% c(7:12),
                                           1, default = 0))]
    }
    
    #Printar algumas informações de salário e emprego em 2014/2
    if(y == 2014 & s == 2){
      emp = tibble(total = sum(rais_s$privado),
                   meio_periodo = sum(rais_s$meio_periodo),
                   temporario = sum(rais_s$temporario))
      sal = tibble(total = mean(rais_s$valor_remuneracao_media[rais_s$privado ==1]),
                   meio_periodo =
                     mean(rais_s$valor_remuneracao_media[rais_s$meio_periodo ==1]),
                   temporario = 
                     mean(rais_s$valor_remuneracao_media[rais_s$temporario == 1]))
      
      print(emp)
      print(sal)
    }
    
    ##Agrupar por tipo de emprego
    privado = rais_s[privado == 1, 
                     .(emprego_privado = sum(empregado)
                       ,tenure_privado = mean(tempo_emprego)
                       ,salario_privado = mean(valor_remuneracao_media)
                       # salario_privado_cbo = mean(mean_wage_3dig),
                       # admitidos_privado = sum(admitido),
                       # demitidos_privado = sum(demitido),
                       # demitidos_total_privado = sum(demitido_total)
                     ),
                     by = .(id_municipio)]
    
    privado_lths = rais_s[privado == 1 & grau_instrucao_apos_2005 < 7, 
                          .(emprego_lths = sum(empregado) 
                            ,tenure_lths = mean(tempo_emprego) 
                            ,salario_lths = mean(valor_remuneracao_media)
                            # ,salario_lths_cbo = mean(mean_wage_3dig)
                          ),
                          by = .(id_municipio)]
    
    privado_hs = rais_s[privado == 1 & grau_instrucao_apos_2005 >=7, 
                        .(emprego_hs = sum(empregado) 
                          ,tenure_hs = mean(tempo_emprego) 
                          ,salario_hs = mean(valor_remuneracao_media)
                          # ,salario_hs_cbo = mean(mean_wage_3dig)
                        ),
                        by = .(id_municipio)]
    
    # privado_baixo_sal = rais_s[privado == 1 & valor_remuneracao_media <= 3000, 
    #                     .(emprego_baixo_sal = sum(empregado), 
    #                       tenure_baixo_sal = mean(tempo_emprego), 
    #                       salario_baixo_sal = mean(valor_remuneracao_media)),
    #                     by = .(id_municipio)]
    # 
    # privado_alto_sal = rais_s[privado == 1 & valor_remuneracao_media > 3000, 
    #                            .(emprego_alto_sal = sum(empregado), 
    #                              tenure_alto_sal = mean(tempo_emprego), 
    #                              salario_alto_sal = mean(valor_remuneracao_media)),
    #                            by = .(id_municipio)]
    
    
    
    # privado_brancos = rais_s[privado == 1 & raca_cor == 2,
    #                          .(emprego_privado_brancos = sum(empregado),
    #                            tenure_privado_brancos = mean(tempo_emprego),
    #                            salario_privado_brancos = mean(valor_remuneracao_media)),
    #                          by = .(id_municipio)]
    # 
    # privado_nao_brancos = rais_s[privado == 1 & !is.na(raca_cor) & !raca_cor %in% c(2,9),
    #                              .(emprego_privado_nao_brancos = sum(empregado),
    #                                tenure_privado_nao_brancos = mean(tempo_emprego),
    #                                salario_privado_nao_brancos = mean(valor_remuneracao_media)),
    #                              by = .(id_municipio)]
    
    publico = rais_s[publico == 1, 
                     .(emprego_publico = sum(empregado),
                       tenure_publico = mean(tempo_emprego),
                       salario_publico = mean(valor_remuneracao_media)),
                     by =.(id_municipio)]
    
    temporario = rais_s[temporario == 1,
                        .(emprego_temporario = sum(empregado),
                          tenure_temporario = mean(tempo_emprego),
                          salario_temporario = mean(valor_remuneracao_media)),
                        by = .(id_municipio)]
    
    temporario_lths = rais_s[temporario == 1 & grau_instrucao_apos_2005 < 7,
                             .(emprego_temporario_lths = sum(empregado),
                               tenure_temporario_lths = mean(tempo_emprego),
                               salario_temporario_lths = mean(valor_remuneracao_media)),
                             by = .(id_municipio)]
    
    temporario_hs = rais_s[temporario == 1 & grau_instrucao_apos_2005 >= 7,
                           .(emprego_temporario_hs = sum(empregado),
                             tenure_temporario_hs = mean(tempo_emprego),
                             salario_temporario_hs = mean(valor_remuneracao_media)),
                           by = .(id_municipio)]
    
    n_temporario = rais_s[n_temporario == 1,
                          .(emprego_n_temporario = sum(empregado),
                            tenure_n_temporario = mean(tempo_emprego),
                            salario_n_temporario = mean(valor_remuneracao_media)),
                          by = .(id_municipio)]
    
    meio_periodo = rais_s[meio_periodo == 1,
                          .(emprego_meio_periodo = sum(empregado),
                            tenure_meio_periodo = mean(tempo_emprego),
                            salario_meio_periodo = mean(valor_remuneracao_media)),
                          by = .(id_municipio)]
    
    
    
    meio_periodo_lths = rais_s[meio_periodo == 1 & grau_instrucao_apos_2005 < 7,
                               .(emprego_meio_periodo_lths = sum(empregado),
                                 tenure_meio_periodo_lths = mean(tempo_emprego),
                                 salario_meio_periodo_lths = mean(valor_remuneracao_media)),
                               by = .(id_municipio)]
    
    meio_periodo_hs = rais_s[meio_periodo == 1 & grau_instrucao_apos_2005 >=7,
                             .(emprego_meio_periodo_hs = sum(empregado),
                               tenure_meio_periodo_hs = mean(tempo_emprego),
                               salario_meio_periodo_hs = mean(valor_remuneracao_media)),
                             by = .(id_municipio)]
    
    
    privado_full = rais_s[privado_full == 1,
                          .(emprego_privado_full = sum(empregado),
                            tenure_privado_full = mean(tempo_emprego),
                            salario_privado_full = mean(valor_remuneracao_media)),
                          by = .(id_municipio)]
    
    
    rural = rais_s[rural == 1,
                   .(emprego_rural = sum(empregado),
                     tenure_rural = mean(tempo_emprego),
                     salario_rural = mean(valor_remuneracao_media)),
                   by = .(id_municipio)]
    
    
    #Juntar informações
    municipios = data.table(id_municipio = unique(rais$id_municipio))
    combinado = merge(municipios, privado,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, privado_lths,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, privado_hs,
                      by = "id_municipio", all.x = TRUE)
    
    # combinado = merge(combinado, privado_baixo_sal,
    #                   by = "id_municipio", all.x = TRUE)
    # 
    # combinado = merge(combinado, privado_alto_sal,
    #                   by = "id_municipio", all.x = TRUE)
    
    # combinado = merge(combinado, privado_brancos,
    #                   by = "id_municipio", all.x = TRUE)
    # 
    # combinado = merge(combinado, privado_nao_brancos,
    #                   by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, publico,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, temporario,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, temporario_lths,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, temporario_hs,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, n_temporario,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, meio_periodo,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, meio_periodo_lths,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, meio_periodo_hs,
                      by = "id_municipio", all.x = TRUE)
    
    combinado = merge(combinado, privado_full,
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
