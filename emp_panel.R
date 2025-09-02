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
#aux datasets
precos = read_excel("../data/deflator_inpc.xlsx", sheet = "anual_junho")
cbo = read_parquet("../data/cbo_3digs.parquet")
rd_index = read_excel("../data/oecd_index.xlsx", sheet = 'cnae_index')
share_col = read_parquet("../data/share_college_cnae.parquet")
setDT(precos)
precos[, indice :=NULL]

#dados de microrregiao
micro = read_excel('../data/regioes_geograficas.xlsx') %>% data.table()
micro = micro[, .(CD_GEOCODI, cod_rgi, nome_mun)]
colnames(micro) = c("id_municipio", 'rgi', 'nome_mun')
micro[, `:=`(id_municipio = as.integer(id_municipio),
             rgi = as.integer(rgi))]

rais_agg = data.table()
for(y in years){
  #Abrir arquivo para esse estado/ano
  filename = paste0(rais_path, "rais_",y,".parquet")
  rais = read_parquet(filename) %>% data.table()
  rais[, natureza_juridica := as.integer(natureza_juridica)]
  
  #corrigir salarios para inflacao (usar deflator de junho de cada ano)
  rais = merge(rais, precos, by = "ano", all.x = TRUE)
  rais[, salario := valor_remuneracao_media * deflator_24]
  
  #adicionar dados de microrregiao
  rais[, id_municipio := as.integer(id_municipio)]
  rais = merge(rais, micro, by = 'id_municipio', all.x = TRUE)
  
  #adicionar salario medio e rank por cbo
  rais[, cbo_3dig := substr(rais$cbo_2002, 1, 3)]
  rais = merge(rais, cbo, by = 'cbo_3dig', all.x = TRUE)
  
  #adicionar grupos de idade
  rais[, grupo_idade := fcase(idade <= 30, 1,
                              idade > 30 & idade < 45, 2,
                              idade > 45, 3, default = NA)]
  
  #adicionar dados de research intensity
  rais[, cnae := substr(cnae_2, 1, 2)]
  rais[, cnae3 := substr(cnae_2, 1, 3)]
  rais = merge(rais, rd_index, by = 'cnae', all.x = TRUE)
  
  #corrigir research intensity para algumas categorias estudadas ao nível de 3 digs
  rais[, rd_intensity := fcase(cnae3 == 303, 31.69,
                               cnae3 == 582, 28.94,
                               cnae3 == 252, 18.87,
                               cnae3 == 325, 9.29,
                               cnae3 == 301, 2.99,
                               cnae3 == 581, 0.57,
                               default = rd_intensity)]
  
  rais[, intensity_cat := fcase(cnae3 == 303, 'High',
                               cnae3 == 582, 'High',
                               cnae3 == 252, 'Mid-High',
                               cnae3 == 325, 'Mid-High',
                               cnae3 == 301, 'Mid',
                               cnae3 == 581, 'Mid-Low',
                               default = intensity_cat)]
  
  #criar categorias de skill intensity mais agregadas
  rais[, new_intensity := fcase(
    intensity_cat %in% c("Low", "Mid-Low"), "low",
    intensity_cat %in% c("Mid", "Mid-High", "High"), "high",
    default = NA
  )]
  
  
  #adicionar dados de share de trabalhadores com college degree por setor
  rais = merge(rais, share_col, by = 'cnae', all.x = TRUE)
  
  #print share of workers by cbo status and intensity category
  if(y == 2014){
    teste = count(rais, rank_wage_cbo) %>% 
      mutate(share = n/sum(n)) %>% 
      print()
    
    count(rais, new_intensity) %>% 
      filter(!is.na(new_intensity)) %>% 
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
      demitido_baixo_sal = fcase(mes_desligamento <= mes_aux
                               & mes_desligamento >= min_mes_semestre
                               & privado == 1
                               & salario <= 2000, 1, default = 0),
      demitido_med_sal = fcase(mes_desligamento <= mes_aux
                                 & mes_desligamento >= min_mes_semestre
                                 & privado == 1
                                 & salario > 2000 & salario <= 6000, 1, default = 0),
      demitido_alto_sal = fcase(mes_desligamento <= mes_aux
                                 & mes_desligamento >= min_mes_semestre
                                 & privado == 1
                                 & salario > 6000 & salario <= 10000, 1, default = 0),
      demitido_altissimo_sal = fcase(mes_desligamento <= mes_aux
                                 & mes_desligamento >= min_mes_semestre
                                 & privado == 1
                                 & salario >10000, 1, default = 0),

      
      admitido_privado = fcase(mes_admissao <= mes_aux
                               & mes_admissao >= min_mes_semestre
                               & privado == 1, 1, default = 0),
      admitido_baixo_sal = fcase(mes_admissao <= mes_aux
                               & mes_admissao >= min_mes_semestre
                               & privado == 1
                               & salario <= 2000, 1, default = 0),
      admitido_med_sal = fcase(mes_admissao <= mes_aux
                                 & mes_admissao >= min_mes_semestre
                                 & privado == 1
                                 & salario > 2000 & salario <= 6000, 1, default = 0),
      admitido_alto_sal = fcase(mes_admissao <= mes_aux
                                 & mes_admissao >= min_mes_semestre
                                 & privado == 1
                                 & salario > 6000 & salario <= 10000, 1, default = 0),
      admitido_altissimo_sal = fcase(mes_admissao <= mes_aux
                                 & mes_admissao >= min_mes_semestre
                                 & privado == 1
                                 & salario > 10000  , 1, default = 0)
      )] 
    rais_alt = rais_alt[, .(demitido = sum(demitido_privado)
                            , demitido_baixo_sal = sum(demitido_baixo_sal)
                            , demitido_med_sal = sum(demitido_med_sal)
                            , demitido_alto_sal = sum(demitido_alto_sal)
                            , demitido_altissimo_sal = sum(demitido_altissimo_sal)
                            , admitido = sum(admitido_privado)
                            , admitido_baixo_sal = sum(admitido_baixo_sal)
                            , admitido_med_sal = sum(admitido_med_sal)
                            , admitido_alto_sal = sum(admitido_alto_sal)
                            , admitido_altissimo_sal = sum(admitido_altissimo_sal)
                            ),
                        by = .(rgi)]
    
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
    
    rais_s[, aux_sum := 1]
    
    
    
    ##Agrupar por tipo de emprego
    privado = rais_s[privado == 1, 
                     .(emprego_privado = .N
                       ,tenure_privado = mean(tempo_emprego)
                       ,salario_privado = mean(salario)
                     ),
                     by = .(rgi)]
    
    homens = rais_s[privado == 1 & sexo == "1", 
                    .(emprego_homens= .N
                      ,tenure_homens = mean(tempo_emprego)
                      ,salario_homens = mean(salario)
                    ),
                    by = .(rgi)]
    
    mulheres = rais_s[privado == 1 & sexo == "2", 
                      .(emprego_mulheres = .N
                        ,tenure_mulheres = mean(tempo_emprego)
                        ,salario_mulheres = mean(salario)
                      ),
                      by = .(rgi)]
    
    h_intensity = rais_s[privado == 1 & new_intensity == "high", 
                      .(emprego_hintensity = .N
                        ,salario_hintensity = mean(salario)
                      ),
                      by = .(rgi)]
    
    l_intensity = rais_s[privado == 1 & new_intensity == "low", 
                         .(emprego_lintensity = .N
                           ,salario_lintensity = mean(salario)
                         ),
                         by = .(rgi)]
    
    privado_lths = rais_s[privado == 1 & grau_instrucao_apos_2005 < 7, 
                          .(emprego_lths = .N 
                            ,tenure_lths = mean(tempo_emprego) 
                            ,salario_lths = mean(salario)
                            , emprego_lths_baixo = sum(aux_sum[salario <= 2000])
                            ,emprego_lths_med = sum(aux_sum[salario > 2000 & salario <= 6000])
                            ,emprego_lths_alto = sum(aux_sum[salario > 6000 & salario <= 10000])
                            ,emprego_lths_altissimo = sum(aux_sum[salario > 10000])
                          ),
                          by = .(rgi)]
    
    privado_hs_somecol = rais_s[privado == 1 & grau_instrucao_apos_2005  %in% c(7,8), 
                                .(emprego_hs_somecol = .N
                                  ,tenure_hs_somecol = mean(tempo_emprego) 
                                  ,salario_hs_somecol = mean(salario)
                                  , emprego_hs_baixo = sum(aux_sum[salario <= 2000])
                                  ,emprego_hs_med = sum(aux_sum[salario > 2000 & salario <= 6000])
                                  ,emprego_hs_alto = sum(aux_sum[salario > 6000 & salario <= 10000])
                                  ,emprego_hs_altissimo = sum(aux_sum[salario > 10000])
                                ),
                                by = .(rgi)]
    
    privado_col = rais_s[privado == 1 & grau_instrucao_apos_2005  >8, 
                         .(emprego_col = .N
                           ,tenure_col = mean(tempo_emprego) 
                           ,salario_col = mean(salario)
                           ,emprego_col_baixo = sum(aux_sum[salario <= 2000])
                           ,emprego_col_med = sum(aux_sum[salario > 2000 & salario <= 6000])
                           ,emprego_col_alto = sum(aux_sum[salario > 6000 & salario <= 10000])
                           ,emprego_col_altissimo = sum(aux_sum[salario > 10000])
                         ),
                         by = .(rgi)]
    
    low_share_col = rais_s[privado == 1 & cat_college  == 1, 
                         .(emprego_lcol = .N
                           ,salario_lcol = mean(salario)
                         ),
                         by = .(rgi)]
    
    mid_share_col = rais_s[privado == 1 & cat_college  == 2, 
                           .(emprego_mcol = .N
                             ,salario_mcol = mean(salario)
                           ),
                           by = .(rgi)]
    
    high_share_col = rais_s[privado == 1 & cat_college  == 3, 
                           .(emprego_hcol = .N
                             ,salario_hcol = mean(salario)
                           ),
                           by = .(rgi)]
    
    privado_baixo_sal = rais_s[privado == 1 & salario <= 2000,
                               .(emprego_baixo_sal = .N,
                                 tenure_baixo_sal = mean(tempo_emprego)),
                               by = .(rgi)]
    
    privado_med_sal = rais_s[privado == 1 & salario > 2000 & salario <= 6000,
                             .(emprego_med_sal = .N,
                               tenure_med_sal = mean(tempo_emprego)),
                             by = .(rgi)]
    
    privado_alto_sal = rais_s[privado == 1 & salario > 6000 & salario <= 10000,
                              .(emprego_alto_sal = .N,
                                tenure_alto_sal = mean(tempo_emprego)),
                              by = .(rgi)]
    
    privado_altissimo_sal = rais_s[privado == 1 & salario > 10000 ,
                              .(emprego_altissimo_sal = .N,
                                tenure_altissimo_sal = mean(tempo_emprego)),
                              by = .(rgi)]
    
    
    privado_sal_acumulado = rais_s[privado == 1,
                                   .(emprego_1 = sum(aux_sum[salario >= 1000]),
                                     emprego_2 = sum(aux_sum[salario >= 2000]),
                                     emprego_3 = sum(aux_sum[salario >= 3000]),
                                     emprego_4 = sum(aux_sum[salario >= 4000]),
                                     emprego_5 = sum(aux_sum[salario >= 5000]),
                                     emprego_6 = sum(aux_sum[salario >= 6000]),
                                     emprego_7 = sum(aux_sum[salario >= 7000]),
                                     emprego_8 = sum(aux_sum[salario >= 8000]),
                                     emprego_9 = sum(aux_sum[salario >= 9000]),
                                     emprego_10 = sum(aux_sum[salario >= 10000]),
                                     emprego_11 = sum(aux_sum[salario >= 11000]),
                                     emprego_12 = sum(aux_sum[salario >= 12000]),
                                     emprego_13 = sum(aux_sum[salario >= 13000]),
                                     emprego_14 = sum(aux_sum[salario >= 14000])
                                     ),
                                   by = .(rgi)]
    
    baixo_cbo = rais_s[privado == 1 & rank_wage_cbo == 4,
                       .(emprego_baixo_cbo = .N,
                         salario_baixo_cbo = mean(salario)),
                       by = .(rgi)]
    
    med_cbo = rais_s[privado == 1 & rank_wage_cbo == 3,
                     .(emprego_med_cbo = .N,
                       salario_med_cbo = mean(salario)),
                     by = .(rgi)]
    
    alto_cbo = rais_s[privado == 1 & rank_wage_cbo == 2,
                      .(emprego_alto_cbo = .N,
                        salario_alto_cbo = mean(salario)),
                      by = .(rgi)]
    
    
    altissimo_cbo = rais_s[privado == 1 & rank_wage_cbo == 1,
                           .(emprego_altissimo_cbo = .N,
                             salario_altissimo_cbo = mean(salario)),
                           by = .(rgi)]
    
    idade_baixo = rais_s[privado == 1 & grupo_idade == 1,
                         .(emprego_baixo_idade = .N,
                           salario_baixo_idade = mean(salario)),
                         by = .(rgi)]
    
    idade_med= rais_s[privado == 1 & grupo_idade == 2,
                         .(emprego_med_idade = .N,
                           salario_med_idade = mean(salario)),
                         by = .(rgi)]
    
    idade_alto = rais_s[privado == 1 & grupo_idade == 3,
                         .(emprego_alto_idade = .N,
                           salario_alto_idade = mean(salario)),
                         by = .(rgi)]
    
    
    publico = rais_s[publico == 1, 
                     .(emprego_publico = .N,
                       tenure_publico = mean(tempo_emprego),
                       salario_publico = mean(salario)),
                     by =.(rgi)]
    
    temporario = rais_s[temporario == 1,
                        .(emprego_temporario = .N,
                          tenure_temporario = mean(tempo_emprego),
                          salario_temporario = mean(salario)),
                        by = .(rgi)]
    
    meio_periodo = rais_s[meio_periodo == 1,
                          .(emprego_meio_periodo = .N,
                            tenure_meio_periodo = mean(tempo_emprego),
                            salario_meio_periodo = mean(salario)),
                          by = .(rgi)]
    
    
    
    rural = rais_s[rural == 1,
                   .(emprego_rural = .N,
                     tenure_rural = mean(tempo_emprego),
                     salario_rural = mean(salario)),
                   by = .(rgi)]
    
    
    #Juntar informações
    rgis = data.table(rgi = unique(rais$rgi))
    combinado = merge(rgis, privado,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, rais_alt,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, homens,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, mulheres,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, h_intensity,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, l_intensity,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, privado_lths,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, privado_hs_somecol,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, privado_col,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, low_share_col,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, mid_share_col,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, high_share_col,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, privado_baixo_sal,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, privado_med_sal,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, privado_alto_sal,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, privado_altissimo_sal,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, privado_sal_acumulado,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, baixo_cbo,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, med_cbo,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, alto_cbo,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, altissimo_cbo,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, idade_baixo,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, idade_med,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, idade_alto,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, publico,
                      by = "rgi", all.x = TRUE)
    
    combinado = merge(combinado, temporario,
                      by = "rgi", all.x = TRUE)
    
    
    combinado = merge(combinado, meio_periodo,
                      by = "rgi", all.x = TRUE)
    
    
    combinado = merge(combinado, rural,
                      by = "rgi", all.x = TRUE)
    
    
    combinado[, anosem := as.numeric(paste0(y,s))]
    
    #juntar no rais_agg
    rais_agg = rbind(rais_agg, combinado)
    
    rm(rais_s)
  }
  rm(rais)
  print(y)
  gc()
}

#garantir uma unica observacao por regiao por periodo
rais_agg[, count := seq_len(.N), by = c("rgi", "anosem")]
rais_agg = rais_agg[count == 1]
rais_agg[, count := NULL]

#trocar ordem das colunas para alfabetica
nova_ordem = sort(colnames(rais_agg))
rais_agg = rais_agg[, ..nova_ordem]
rm(nova_ordem)

####Criar painel balanceado
bal_panel = expand_grid(
  rgi = unique(rais_agg$rgi),
  anosem = unique(rais_agg$anosem)) %>%
  mutate(ano = as.integer(substr(anosem, 1,4))) %>% 
  data.table()

bal_panel = merge(bal_panel, rais_agg,
                  by = c("rgi", "anosem"),
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
