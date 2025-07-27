


######################################
#Tentativa Sun and Abraham com pesos
######################################
library(fixest)

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



#abrir dataset
df = read_parquet("../data/ub_rais_merged.parquet")

#parametros:
#dropar primeiros municipios
dropar_primeiros_munics = 1
minimo_cidades = 10
minimo_habs = 50000

df = df[!(semestre_entrada %in% c(20141, 20142, 20151, 20152))]
df[, conta_cidade_grupo := length(unique(id_municipio)), by = .(semestre_entrada_did)]
df = df[conta_cidade_grupo >=minimo_cidades]
df = df[pop14 >= minimo_habs]
df[,uf := substr(id_municipio,1, 2)]
df[,region := substr(id_municipio, 1,1)]
df[, uft := paste0(uf, ano)]
df[, tratado := fifelse(!is.na(semestre_entrada), 1, 0)]

df_est = df[anosem == 20142]
#dataset ao nível da microrregiao
df[, manter := fifelse(id_municipio == min(id_municipio),1, 0),
            by = 'rgi']
df_mmc = df[manter == 1 & anosem == 20142]


#Estimar propensity score
ps_model = glm(tratado ~  lincome_r
               + lpop_r
               # + unem_rate_m
               + unem_rate_r
               # + inf_rate_m
               + inf_rate_r
               + lpibpc_r
               + lemployed_r
               # + lpop
               # + lmean_pop_r
               + age_r
               + factor(uf)
               , data = df_est,
              family = 'binomial')


print(summary(ps_model))
df_est[, prob := predict(ps_model, type = 'response')]
df_est[, tratado := as.character(tratado)]


#Adicionar ao df principal
df_est = df_est[, .(id_municipio, prob)]
df = merge(df, df_est, by = 'id_municipio', all.x = TRUE)

#limpar probs muito altas ou muito baixas
df = df[prob >= 0.001 & prob <= 0.999]

#contar municípios por tratamento
count(df[anosem == 20142], tratado)

#focar nos períodos que queremos ver
df[, anosem_did_relativo := fifelse(
  tratado == 1
  , anosem_did - semestre_entrada_did,
  0)]

df = df[anosem_did_relativo >= -7 & anosem_did_relativo <= 6]

#plot distribution of propensity scores among treated and control municipalities
# ggplot(df[anosem == 20142],
#        aes(prob, colour = tratado, group = tratado))+geom_density()

#criar pesos
df[, peso := fifelse(
  tratado == 1, 1,
  prob/(1-prob)
)]



m = feols(log(salario_lths) ~ sunab(semestre_entrada_did, anosem_did) 
          | id_municipio + anosem_did +uft ,
          data = df,
          weights = ~peso,
          cluster = 'rgi')

iplot(m)


