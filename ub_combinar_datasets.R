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
                admissao =  sum(admitido), 
                demissao = sum(demitido),
                emprego_homens = sum(emprego_homens),
                emprego_mulheres = sum(emprego_mulheres),
                emprego_lths = sum(emprego_lths),
                emprego_hs_somecol = sum(emprego_hs_somecol),
                emprego_col = sum(emprego_col),
                emprego_baixo_sal = sum(emprego_baixo_sal),
                emprego_med_sal = sum(emprego_med_sal),
                emprego_alto_sal = sum(emprego_alto_sal),
                emprego_altissimo_sal = sum(emprego_altissimo_sal),
                emprego_baixo_cbo = sum(emprego_baixo_cbo),
                emprego_med_cbo = sum(emprego_med_cbo),
                emprego_alto_cbo = sum(emprego_alto_cbo),
                emprego_altissimo_cbo = sum(emprego_altissimo_cbo),
                emprego_baixo_idade = sum(emprego_baixo_idade),
                emprego_med_idade = sum(emprego_med_idade),
                emprego_alto_idade = sum(emprego_alto_idade),
                emprego_meio_periodo = sum(emprego_meio_periodo),
                emprego_temporario = sum(emprego_temporario),
                emprego_publico = sum(emprego_publico),
                emprego_rural = sum(emprego_rural),
                salario_privado = weighted.mean(salario_privado, emprego_privado, na.rm = TRUE),
                salario_homens = weighted.mean(salario_homens, emprego_homens, na.rm = TRUE),
                salario_mulheres = weighted.mean(salario_mulheres, emprego_mulheres, na.rm = TRUE),
                salario_lths = weighted.mean(salario_lths, emprego_lths, na.rm = TRUE),
                salario_hs_somecol = weighted.mean(salario_hs_somecol, emprego_hs_somecol, na.rm = TRUE),
                salario_col = weighted.mean(salario_col, emprego_col, na.rm = TRUE),
                salario_baixo_cbo= weighted.mean(salario_baixo_cbo, emprego_baixo_cbo, na.rm = TRUE),
                salario_med_cbo = weighted.mean(salario_med_cbo, emprego_med_cbo, na.rm = TRUE),
                salario_alto_cbo = weighted.mean(salario_alto_cbo, emprego_alto_cbo, na.rm = TRUE),
                salario_altissimo_cbo = weighted.mean(salario_altissimo_cbo, emprego_altissimo_cbo, na.rm = TRUE),
                salario_baixo_idade = weighted.mean(salario_baixo_idade, emprego_baixo_idade, na.rm = TRUE),
                salario_med_idade = weighted.mean(salario_med_idade, emprego_med_idade, na.rm = TRUE),
                salario_alto_idade = weighted.mean(salario_alto_idade, emprego_alto_idade, na.rm = TRUE),
                salario_publico = weighted.mean(salario_publico, emprego_publico, na.rm = TRUE),
                salario_rural = weighted.mean(salario_rural, emprego_rural, na.rm = TRUE)
               
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
populacao = populacao[, .(pop = sum(pop), pop_max = max(pop)), by = .(ano, rgi)]

rais = merge(rais, populacao, by = c('rgi', 'ano'), all.x = TRUE)

#populacao em 2014, logo antes do uber chegar
rais[, pop_14 := pop[anosem == 20142], by = 'rgi']
rais[, pop_14_max := pop_max[anosem == 20142], by = 'rgi']

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
            lpop = log(pop),
            lpop_max = log(pop_14_max))]


###################
#Frota de veículos
###################
frota = read_parquet("../data/FROTA.parquet")
setDT(frota)
frota = frota[mes == 6 | mes == 12]
frota[, `:=`(id_municipio = as.integer(id_municipio),
             semestre = fifelse(mes == 6, 1, 2))]
frota[, anosem := as.integer(paste0(ano,semestre))]

#adicionar dados de regiao
frota = merge(frota, micro, by = 'id_municipio', all.x = TRUE)
frota = frota[!is.na(rgi)]

#agregar por regiao e adicionar no df principal
frota = frota[, .(n_veics = sum(quantidade)), by = .(rgi, anosem)]
rais = merge(rais, frota, by = c("rgi", "anosem"), all.x = TRUE)


###################
#Nascimentos
###################
nascimentos = read_parquet("../data/nascimentos.parquet") %>% data.table()
nascimentos[, `:=`(id_municipio = as.integer(id_municipio_nascimento),
                   anosem = fcase(mes <= 6, paste0(ano,1),
                                  mes >6 & mes <= 12, paste0(ano,2),
                                  default = NA))]
#adicionar dados de regiao
nascimentos = merge(nascimentos, micro, by = 'id_municipio', all.x = TRUE)
nascimentos = nascimentos[!is.na(rgi)]

#agregar por regiao e adicionar no df principal
nascimentos = nascimentos[, .(nascimentos = sum(total_nascimentos)),
                          by = .(rgi, anosem)]
nascimentos[, anosem := as.integer(anosem)]

rais = merge(rais, nascimentos, by = c("rgi", "anosem"), all.x = TRUE)


#########################################
#Tax collection
#########################################
ir = read_excel('../Data/arrecadacao_ir.xlsx', skip = 6) %>% data.table()
colnames(ir) = c('ano', 'uf', 'nome_municipio', 'irpf', 'qtdpf', 'irpj', 'qtdpj'
                 ,'irtotal', 'qtdtotal' )

#transformar colunas de valores em inteiros
cols_change = c('irpf', 'irpj', 'irtotal', 'qtdpf', 'qtdpj', 'qtdtotal')
ir[, (cols_change) := lapply(.SD, function(x) x = as.numeric(x)), .SDcols = cols_change]
ir = ir[!is.na(nome_municipio) & ano > "2010"]

#adicionar info de estado à micro
micro[, uf := as.integer(substr(id_municipio, 1, 2))]
micro[, nome_uf := fcase(uf == 11, 'RO', 
                         uf == 12, 'AC',
                         uf == 13, 'AM',
                         uf == 14, 'RR',
                         uf == 15, 'PA',
                         uf == 16, 'AP',
                         uf == 17, 'TO',
                         uf == 21, 'MA',
                         uf == 22, 'PI', 
                         uf == 23, 'CE',
                         uf == 24, 'RN',
                         uf == 25, 'PB',
                         uf == 26, 'PE',
                         uf == 27, 'AL',
                         uf == 28, 'SE',
                         uf == 29, 'BA',
                         uf == 31, 'MG',
                         uf == 32, 'ES',
                         uf == 33, 'RJ',
                         uf == 35, 'SP',
                         uf == 41, 'PR',
                         uf == 42, 'SC',
                         uf == 43, 'RS',
                         uf == 50, 'MS',
                         uf == 51, 'MT',
                         uf == 52, 'GO',
                         uf == 53, 'DF', 
                         default = NA
                         )]
micro[, uf := NULL]

#Adicionar info do nome do municipio no formato "Nome_municipio - CodUF"
micro[, nome_municipio := paste0(nome_mun, " - ", nome_uf)]


#Limpar nome do municipio nas duas bases
library(stringi)
micro[, nome_municipio := stri_trans_general(str = nome_municipio, 
                                             id = 'Latin - ASCII')]
ir[, nome_municipio := stri_trans_general(str = nome_municipio, 
                                          id = 'Latin - ASCII')]

#adicionar dados de microrregiao e agregar
ir = merge(ir, micro, by = 'nome_municipio', all.x = TRUE)

ir = ir[, .(irpf = sum(irpf, na.rm = TRUE), 
            irpj = sum(irpj, na.rm = TRUE),
            irtotal = sum(irtotal, na.rm = TRUE),
            qtdpf = sum(qtdpf, na.rm = TRUE),
            qtdpj = sum(qtdpj, na.rm = TRUE),
            qtdtotal =sum(qtdtotal, na.rm = TRUE)),
        by = .(rgi, ano)]

ir[, ano := as.integer(ano)]

#merge to main database
rais = merge(rais, ir, by = c('rgi', 'ano'), all.x = TRUE)

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
