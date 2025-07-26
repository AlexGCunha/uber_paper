
library(devtools)
library(cowplot)
library(tidyverse)
library(arrow)
library(did)
library(data.table)
library(fixest)
library(modelsummary)
library(HonestDiD)
library(BMisc)
library(DIDmultiplegtDYN)


#abrir dataset
df = read_parquet("../data/ub_rais_merged.parquet")

#parametros:
#dropar primeiros municipios
dropar_primeiros_munics = 1
minimo_cidades = 10
maximo_periodo = 20192
# minimo_habs = 50000

#definir controles
controles = as.formula(" ~ lmean_income+ unem_rate_m + inf_rate_m + lpop_t")
sem_controles = as.formula("~ 1")

######################################
#Ajustes
######################################
#Dropar primeiros municípios e ultimos municipios
if(dropar_primeiros_munics == 1){
  df = df[!(tem_uber == 1 & semestre_entrada %in% c(20141, 20142, 20151, 20152))]
}

df = df[anosem <= maximo_periodo]

#dropar grupos onde o numero de cidades tratadas no periodo é menor que o minimo
df[, conta_cidade_grupo := length(unique(id_municipio)), by = .(semestre_entrada_did)]
df = df[conta_cidade_grupo >=minimo_cidades]



source("../uber2/ub_funcoes_auxiliares.R")
dia = "202504"


######################
#Ajustes para o pacote do chaisemartin
######################
chaise = copy(df)
chaise[, tratado := fifelse(
  tem_uber== 1 & anosem_did >= semestre_entrada_did, 1, 0)]
chaise[, `:=`(lemprego  = log(emprego_privado),
              lemprego_lths = log(emprego_lths),
              lemprego_hs = log(emprego_hs))] 
chaise = chaise[emprego_lths > 0]

mc = did_multiplegt_dyn(df = chaise,
                        outcome = 'lemprego',
                        group = 'id_municipio', 
                        time = 'anosem_did',
                        treatment = 'tratado',
                        effects = 5, placebo = 5, cluster = 'id_municipio',
                        controls = c('lpop_t'))

