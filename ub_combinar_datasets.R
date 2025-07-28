################################################################################
#Esse código irá:
#- limpar os dados de emprego e 
################################################################################
library(tidyverse)
library(data.table)
library(arrow)
library(readxl)

###################
#RAIS
###################
rais = read_parquet("../data/ub_painel_emprego_basico.parquet") %>% data.table()
rais[, id_municipio := as.integer(id_municipio)]
rais = rais[!is.na(id_municipio)]

#adicionar dados de mmc
micro = read_excel('../data/regioes_geograficas.xlsx') %>% data.table()
micro = micro[, .(CD_GEOCODI, cod_rgi, nome_mun)]
colnames(micro) = c("id_municipio", 'rgi', 'nome_mun')
micro[, `:=`(id_municipio = as.integer(id_municipio),
             rgi = as.integer(rgi))]
rais = merge(rais, micro, by = 'id_municipio', all.x = TRUE)

#Agregar dados ao nível da regiao
rais = rais[, .(ano = first(ano), 
                emprego_privado = sum(emprego_privado),
                emprego_hs = sum(emprego_hs),
                emprego_lths = sum(emprego_lths),
                emprego_meio_periodo = sum(emprego_meio_periodo),
                emprego_meio_periodo_hs = sum(emprego_meio_periodo_hs),
                emprego_meio_periodo_lths = sum(emprego_meio_periodo_lths),
                emprego_temporario = sum(emprego_temporario),
                emprego_temporario_hs = sum(emprego_temporario_hs),
                emprego_temporario_lths = sum(emprego_temporario_lths),
                emprego_publico = sum(emprego_publico),
                emprego_rural = sum(emprego_rural),
                salario_hs = weighted.mean(salario_hs, emprego_hs, na.rm = TRUE),
                salario_lths = weighted.mean(salario_lths, emprego_lths, na.rm = TRUE),
                salario_privado = weighted.mean(salario_privado, emprego_privado, na.rm = TRUE),
                salario_publico = weighted.mean(salario_publico, emprego_publico, na.rm = TRUE),
                salario_rural = weighted.mean(salario_rural, emprego_rural, na.rm = TRUE),
                salario_full = weighted.mean(salario_privado_full, emprego_privado_full, na.rm = TRUE)
),
by = .(rgi, anosem)]


###################
#DATAS UBER
###################
datas = read_excel("../data/initial_dates.xlsx", sheet = "032023") %>% data.table()
setnames(datas, old = "munic", new = "id_municipio")
colunas_para_manter = c("id_municipio", "semester_entry", "year_entry", "problem")
datas = datas[,..colunas_para_manter]
rm(colunas_para_manter)

#definir anosem de tratamento
datas[, semestre_entrada := as.integer(paste0(year_entry, semester_entry))]

#adicionar dados de mmc
datas = merge(datas, micro, by = 'id_municipio', all.x = TRUE)

#definir semestre de entrada como o mínimo da microrregiao
datas[, semestre_entrada := fifelse(is.na(semestre_entrada), 99999, semestre_entrada)]
datas = datas[, .(semestre_entrada = min(semestre_entrada)), by = 'rgi']

#adicionar ao df principal
rais = merge(rais, datas, by = 'rgi', all.x = TRUE)

#vamos então dropar as regioes que possuem uber e eu n sei quando chegou la
rais = rais[is.na(semestre_entrada) | semestre_entrada != 99999]

#definir tem uber se tem uber na microrregiao
rais[, tratado := fifelse(! is.na(semestre_entrada), 1, 0)]

#Criar indicador de semestre para ano e para quando uber chegou em uma cidade
# -- isso é, um indicador para usar no pacote did de Callaway & Santanna
anosem = sort(unique(rais$anosem))
datas_did = data.table(anosem = anosem,
                       anosem_did = c(1:(length(anosem))))

#incluir data corrente como formato do pacote 
rais = merge(rais, datas_did, by = "anosem", all.x = TRUE)

#incluir data de entrada como formato do pacote
setnames(datas_did, old = c("anosem", "anosem_did"),
         new = c("semestre_entrada", "semestre_entrada_did"))
rais = merge(rais, datas_did, by = "semestre_entrada", all.x = TRUE)

rm(datas, datas_did, anosem)


#Inputar data de entrada did 0 para os municipios que nao tem uber (para o pacote)
rais[, semestre_entrada_did := fifelse(tratado == 0, 0, semestre_entrada_did)]

###################
#População
###################
#Populacao
populacao = read_excel("../data/time_series_pop.xlsx") %>% data.table()
populacao[, `:=`(id_municipio = as.numeric(id_municipio))]
populacao = populacao[, .(id_municipio, `2012`, `2013`, `2014`, `2015`, 
                          `2016`, `2017`,`2018`, `2019`, `2020`)]
populacao = populacao %>% 
  pivot_longer(cols = 2:(ncol(populacao)), names_to = 'ano', values_to = 'pop') %>% 
  data.table()

populacao[, ano := as.integer(ano)]

#adicionar dados de mmc e agregar
populacao = merge(populacao, micro, by = 'id_municipio', all.x = TRUE)
populacao = populacao[, .(pop = sum(pop)), by = .(ano, rgi)]

rais = merge(rais, populacao, by = c('rgi', 'ano'), all.x = TRUE)

#populacao em 2014, logo antes do uber chegar
rais[, pop_14 := pop[anosem == 20142], by = 'rgi']

###################
#Variáveis Censo 2010
###################
censo = read_parquet("../data/rgi_data_10.parquet") %>% data.table()

#adicionar variáveis do censo
rais = merge(rais, censo,by = "rgi", all.x = TRUE)

#dropar regioes sem informacao do censo
rais = rais[!is.na(pop_r)]

#criar variaveis em log
rais[, `:=`(lincome_r = log(mean_income_r),
            lemployed_r = log(employed_r),
            lpop_r = log(pop_r),
            lpea_r = log(pea_r),
            lpop = log(pop))]


###################
#SIS-SUS - Mortalidade do DataSUS
#DEMORA PARA RODAR E EU NÃO ACHEI NENHUM EFEITO, ENTÃO VOU DEIXAR COMENTADO
###################
# 
# sis = read_parquet("../data/SIM.parquet") %>% data.table()
# #Acidantes de transporte terrestre estao entre os CID V01 e V99
# 
# #definir letra e codigos do cid
# sis[, letra := substr(causa_basica, 1,1)]
# sis[, codigo := as.integer(substr(causa_basica, 2,3))]
# sis[, id_municipio := as.integer(substr(id_municipio_ocorrencia, 1, 6))]
# #definir se foi acidente veicular
# sis[, acidente_veicular := fcase(letra == "V" & codigo %in% seq(1,89,1), 1,
#                                  default = 0)]
# 
# #Definir semestre do obito
# sis[, `:=`(mes_obito = as.integer(format(data_obito, '%m')),
#            ano_obito = as.integer(format(data_obito, '%Y')))]
# sis = sis[!is.na(mes_obito)]
# sis[, semestre_obito := fcase(mes_obito <= 6, 1,
#                              default = 2)]
# sis[, anosem := as.integer(paste0(ano_obito, semestre_obito))]
# 
# #contar mortes por homicídio e acidente veicular, por municipio e semestre
# homicidio = sis[circunstancia_obito == "3", .(homicidios = .N),
#                by = .(id_municipio, anosem)]
# 
# acidente = sis[acidente_veicular == 1, .(mortes_acidente_carro = .N),
#           by = .(id_municipio, anosem)]
# homicidio = homicidio[!is.na(id_municipio)]
# acidente = acidente[!is.na(id_municipio)]
# 
# #adicionar dados de mmc
# micro[, id_municipio := as.integer(substr(id_municipio, 1, 6))]
# homicidio = merge(homicidio, micro, by = 'id_municipio', all.x = TRUE)
# acidente = merge(acidente, micro, by = 'id_municipio', all.x = TRUE)
# 
# #agregar ao nível da microrregiao e anosem
# homicidio = homicidio[, .(homicidios = sum(homicidios)), by = .(rgi, anosem)]
# acidente = acidente[, .(mortes_acidente_carro = sum(mortes_acidente_carro)), by = .(rgi, anosem)]
# 
# #merge
# rais = merge(rais, homicidio, by = c("rgi", "anosem"), all.x = TRUE)
# rais = merge(rais, acidente, by = c("rgi", "anosem"), all.x = TRUE)
# 
# rais[, homicidios := fifelse(is.na(homicidios), 0, homicidios)]
# 
# rais[, mortes_acidente_carro := fifelse(is.na(mortes_acidente_carro), 0,
#                                       mortes_acidente_carro)]
# rais[, homicidios_pc := homicidios*100000/(pop)]
# rais[, mortes_acidente_carro_pc := mortes_acidente_carro*100000/(pop)]
# rm(sis, homicidio, acidente)
# 


#Salvar x
write_parquet(rais, "../data/ub_rais_merged.parquet")
rm(list = ls())
gc()
