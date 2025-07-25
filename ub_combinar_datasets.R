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

###################
#DATAS UBER
###################
datas = read_excel("../data/initial_dates.xlsx", sheet = "032023") %>% data.table()
setnames(datas, old = "munic", new = "id_municipio")
colunas_para_manter = c("id_municipio", "semester_entry", "year_entry", "problem")
datas = datas[,..colunas_para_manter]
rm(colunas_para_manter)

#variavel indicadora de que esse municipio possui uber
datas[, tem_uber := 1]
datas[, semestre_entrada := as.numeric(paste0(year_entry, semester_entry))]
datas[, `:=`(semester_entry = NULL, year_entry = NULL)]

#drop duplicates
datas[, count := seq_len(.N), by = .(id_municipio)]
datas = datas[count == 1]
datas[, count := NULL]


#Combinar dados
rais = merge(rais, datas, by = "id_municipio", all.x = TRUE)
setnames(rais, old = "problem", new = "problema_datas")


###################
#Dados de microrregiao 
#a ideia é expandir a data de entrada do uber para todos os municipios na mesma
#microrregiao
###################
micro = read_excel('../data/regioes_geograficas.xlsx') %>% data.table()
micro = micro[, .(CD_GEOCODI, cod_rgi, nome_mun)]
colnames(micro) = c("id_municipio", 'rgi', 'nome_mun')
micro[, `:=`(id_municipio = as.integer(id_municipio),
             rgi = as.integer(rgi))]

rais = merge(rais, micro, by = 'id_municipio', all.x = TRUE)

#definir a data de entrada do uber na microrregiao como a minima dentro dessa microrregiao
rais[, semestre_entrada := fifelse(
  is.na(semestre_entrada), 99999, semestre_entrada)]

rais[, semestre_entrada := min(semestre_entrada), by = 'rgi']


#OU definir a data de entrada do uber na cidade como a minima da microrreriao, so pras cidades sem data
# rais[, semestre_entrada_aux := fifelse(
#   is.na(semestre_entrada), 99999, semestre_entrada)]
# 
# rais[, semestre_entrada_aux := min(semestre_entrada_aux), by = 'rgi']
# rais[, semestre_entrada := fifelse(is.na(semestre_entrada), semestre_entrada_aux,
#                                  semestre_entrada)]
# rais[, semestre_entrada_aux := NULL]


#definir tem uber se tem uber na microrregiao
rais[, tem_uber := fifelse(is.na(tem_uber), 0, tem_uber)]
rais[, tem_uber := max(tem_uber), by = 'rgi']

#Agora eu preciso dropar as microrregies que tem uber e eu não sei a data de
#entrada
rais[, erro := fifelse(
  tem_uber == 1 & semestre_entrada == 99999, 1, 0
)]
rais[, erro := max(erro), by ='rgi']
rais = rais[erro == 0]
rais[, erro := NULL]

#voltar a colocar NA para cidades que n te uber
rais[, semestre_entrada := fifelse(tem_uber == 0, NA, semestre_entrada)]

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
rais[, semestre_entrada_did := fifelse(tem_uber == 0, 0, semestre_entrada_did)]

###################
#População e PIB municipal
###################
#Populacao
populacao = read_excel("../data/pop_mun.xlsx", sheet = "Tabela") %>% data.table()
colunas_manter = c("id_municipio", "2014")
populacao = populacao[, ..colunas_manter]
populacao[, id_municipio := as.integer(id_municipio)]
rm(colunas_manter)
setnames(populacao, old = "2014", new = "pop14")

#combinar
rais = merge(rais, populacao, by = "id_municipio", all.x = TRUE)
rm(populacao)

#PIB Municipal
pibmun = read_parquet("../data/pib_mun.parquet") %>% 
  filter(ano == 2014) %>% 
  data.table()
pibmun = pibmun[, .(id_municipio, pib)]
pibmun[, id_municipio := as.integer(id_municipio)]

#combinar
rais = merge(rais, pibmun, by = "id_municipio", all.x = TRUE)
rais[,pibpc14 := pib/pop14]
rm(pibmun)

###################
#Variáveis Censo 2010
###################
censo = read_parquet("../data/munic_data_10.parquet") %>% data.table()
#corrigir nome de colunas
setnames(censo, old = "munic", new = "id_municipio")
# censo[, pop_m := NULL]
censo[, id_municipio := as.integer(id_municipio)]

#no censo, o id de municipio tem 6 digitos (n tem o 7 digito verificador), 
#-- entao vou tirar o digito verificador da base principal também
rais[, id_municipio := as.integer(substr(id_municipio, 1, 6))]

#garantir uma observacao por municipio por semestre
rais[, count := seq_len(.N), by = .(id_municipio, anosem)]
rais = rais[count == 1]
rais[, count := NULL]

#adicionar variáveis do censo
rais = merge(rais, censo,
             by = "id_municipio", all.x = TRUE)

#dropar cidades sem informacao do censo
rais = rais[!is.na(pea_m)]

#Remover maiores e menores 1% cidades
q1 = quantile(rais$pop14, 0.01)
q99 = quantile(rais$pop14, 0.99)
print(q1)
print(q99)
rais = rais[pop14 > q1 & pop14 < q99]
rm(q1, q99)

#create log pop
rais[, lpop := log(pop_m)]
rais[, lpop2 := log(pop_m)^2]


#Criar variáveis ao nível da microrregiao
rais[, `:=`(mean_income_r = weighted.mean(mean_income_m, employed_m),
            unem_rate_r = weighted.mean(unem_rate_m, pea_m),
            inf_rate_r = weighted.mean(inf_rate_m, employed_m),
            employed_r = mean(employed_m),
            mean_pop_r = mean(pop_m),
            tot_pop_r = sum(pop_m),
            age_r = mean(age_m),
            pibpc_r = weighted.mean(pibpc14, pop_m)), by = .(rgi)]

rais[, `:=`(lincome_m = log(mean_income_m),
            lincome_r = log(mean_income_r),
            lemployed_r = log(employed_r),
            lmean_pop_r = log(mean_pop_r),
            ltot_pop_r = log(tot_pop_r),
            lpibpc_r = log(pibpc_r))]


###################
#SIS-SUS - Mortalidade do DataSUS
#DEMORA PARA RODAR E EU NÃO ACHEI NENHUM EFEITO, ENTÃO VOU DEIXAR COMENTADO
###################

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
# 
# #merge
# rais = merge(rais, homicidio, by = c("id_municipio", "anosem"), all.x = TRUE)
# rais = merge(rais, acidente, by = c("id_municipio", "anosem"), all.x = TRUE)
# 
# rais[, homicidios := fifelse(is.na(homicidios), 0, homicidios)]
# 
# rais[, mortes_acidente_carro := fifelse(is.na(mortes_acidente_carro), 0,
#                                       mortes_acidente_carro)]
# rais[, homicidios_pc := homicidios*100000/(pop14)]
# rais[, mortes_acidente_carro_pc := mortes_acidente_carro*100000/(pop14)]
# rm(sis, homicidio, acidente)

###################
#SIA-SUS - Produção Ambulatorial
# NÃO CONSEGUI ENTENDER COMO OS VALORES GERADOS ESTÃO MUITO BAIXOS,
# ENTÃO NÃO VOU USAR ESSES DADOS
###################
# sia = read_parquet("../data/sus_ambulatorio.parquet") %>% data.table()
# 
# #Definir se, em alguma categoria, temos cid de acidentes de transito
# sia[, `:=`(letra_principal = substr(cid_principal_categoria, 1,1),
#            codigo_principal = as.integer(substr(cid_principal_categoria, 2,3)),
#            letra_secundario = substr(cid_secundario_categoria, 1, 1),
#            codigo_secundario = as.integer(substr(cid_secundario_categoria,2,3)),
#            letra_associado = substr(cid_causas_associadas_categoria,1,1),
#            codigo_associado = as.integer(substr(cid_causas_associadas_categoria,2,3)))]
# 
# sia[, `:=`(acidente_principal = fifelse(letra_principal == 'V' & codigo_principal %in% seq(1, 89,1), 1, 0),
#            acidente_secundario = fifelse(letra_secundario == "V" & codigo_secundario %in% seq(1, 89,1), 1, 0),
#            acidente_associado = fifelse(letra_associado == "V" & codigo_associado %in% seq(1, 89,1), 1, 0))]
# 
# 
# sia[, acidente_veicular := fcase(acidente_principal == 1, 1,
#                                  acidente_secundario == 1, 1,
#                                  acidente_associado == 1, 1,
#                                  default = 0)]
# sia = sia[acidente_veicular == 1]
# 
# sia[, semestre := fifelse(mes <= 6, 1, 2)]
# sia[, anosem := as.integer(paste0(ano, semestre))]
# sia[, id_municipio := as.integer(substr(id_municipio, 1, 6))]
# 
# 
# #corrigir para inflação anual
# precos = read_excel("../data/deflator_inpc.xlsx", sheet = "anual_junho")
# setDT(precos)
# precos[, indice :=NULL]
# sia = merge(sia, precos, by = "ano", all.x = TRUE)
# sia[, valor_producao_real := valor_producao * deflator_10]
# 
# #agregar por municipio e semestre
# sia = sia[, .(prod_amb_acidente_terrestre = sum(valor_producao_real)),
#           by = .(anosem, id_municipio)]
# 
# #merge
# rais = merge(rais, sia, by = c("id_municipio", "anosem"), all.x = TRUE)



#Salvar x
write_parquet(rais, "../data/ub_rais_merged.parquet")
rm(list = ls())
gc()
