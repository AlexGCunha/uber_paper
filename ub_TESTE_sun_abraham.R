######################################
#Regressoes usando Sun and Abraham e De Chaisemartin
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
library(DIDmultiplegtDYN)

######################################
#Modificacoes iniciais
######################################

#abrir dataset
df = read_parquet("../data/ub_rais_merged.parquet")

#parametros:
#dropar primeiros municipios
minimo_cidades = 10

df = df[!(semestre_entrada %in% c(20141, 20142, 20151, 20152))]
df[, conta_cidade_grupo := length(unique(rgi)), by = .(semestre_entrada_did)]
df = df[conta_cidade_grupo >=minimo_cidades]
df[,uf := substr(rgi,1, 2)]
df[,region := substr(rgi, 1,1)]
df_est = df[anosem == 20142]

######################################
#Propensity Score
######################################
#Estimar propensity score
ps_model = glm(tratado ~  lincome_r
               + log(pop_r)
               + unem_rate_r
               + inf_rate_r
               # + lemployed_r
               + age_r
               # + factor(region)
               , data = df_est,
              family = 'binomial')


print(summary(ps_model))
df_est[, prob := predict(ps_model, type = 'response')]
df_est[, tratado := as.character(tratado)]


#Adicionar ao df principal
df_est = df_est[, .(rgi, prob)]
df = merge(df, df_est, by = 'rgi', all.x = TRUE)

#limpar probs muito altas ou muito baixas
df = df[prob >= 0.01 & prob <= 0.99]

#contar municípios por tratamento
count(df[anosem == 20142], tratado)

#focar nos períodos que queremos ver
df[, anosem_did_relativo := fifelse(
  tratado == 1
  , anosem_did - semestre_entrada_did,
  0)]

df = df[anosem_did_relativo >= -7 & anosem_did_relativo <= 6]



#criar pesos
df[, peso := fifelse(
  tratado == 1, 1,
  prob/(1-prob)
)]



m = feols(log(emprego_lths) ~ sunab(semestre_entrada_did, anosem_did) #+log(pop)
          | rgi + anosem_did  ,
          data = df,
          weights = ~peso,
          cluster = 'rgi')

iplot(m)


# ######################
# #Ajustes para o pacote do chaisemartin
# ######################
chaise = copy(df)
chaise[, tratado := fifelse(
  tratado== 1 & anosem_did >= semestre_entrada_did, 1, 0)]
chaise[, `:=`(lemprego  = log(emprego_privado),
              lemprego_lths = log(emprego_lths),
              lemprego_hs = log(emprego_hs),
              lsalario = log(salario_privado),
              lsalario_hs = log(salario_hs),
              lsalario_lths = log(salario_lths))]
chaise = chaise[emprego_lths > 0]

#pegar dummies e interagir com tempo
mc = did_multiplegt_dyn(df = chaise,
                        outcome = 'lemprego',
                        group = 'rgi',
                        time = 'anosem_did',
                        treatment = 'tratado',
                        effects = 6, placebo = 6, cluster = 'rgi',
                        # controls = c('lpop_r_t'),
                        weight =  'peso')
print(mc)

