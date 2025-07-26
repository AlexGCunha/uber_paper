
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
dropar_primeiros_munics = 0
minimo_cidades = 10
maximo_periodo = 20202
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

#Dropar minimo de habitantes
# df = df[pop14 >= minimo_habs]

#dropar grupos onde o numero de cidades tratadas no periodo é menor que o minimo
df[, conta_cidade_grupo := length(unique(id_municipio)), by = .(semestre_entrada_did)]
df = df[conta_cidade_grupo >=minimo_cidades]



source("../uber2/ub_funcoes_auxiliares.R")
dia = "202504"


######################
#Ajustes para o pacote do chaisemartin
######################

