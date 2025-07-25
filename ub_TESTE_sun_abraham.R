


######################################
#Tentativa Sun and Abraham com pesos
######################################
library(fixest)
df = read_parquet("../data/ub_rais_merged.parquet")
df[, uf := substr(id_municipio, 1, 2)]
df[, region := substr(id_municipio, 1,1)]
df[, tratado := fifelse(is.na(semestre_entrada), 0, 1)]
df[, pop2 := pop14^2]
df_est = df[anosem == 20142]


#Estimar propensity score
ps_model = glm(tratado ~  lincome_m
               +lincome_r
               # + unem_rate_m
               + unem_rate_r
               # + inf_rate_m 
               + inf_rate_r
               + lpibpc_r
               + lemployed_r
               # + lpop
               # + lmean_pop_r
               + ltot_pop_r
               + age_m
               + age_r
               + factor(uf)
               , data = df_est,
              family = 'binomial')


# ps_model = lm(tratado ~  lincome_m
#               +lincome_r
#               + unem_rate_m
#               + unem_rate_r
#               + inf_rate_m 
#               + inf_rate_r
#               + log(pibpc14) 
#               + log(employed_m) 
#               + lpop
#               + lmean_pop_r
#               + ltot_pop_r
#               + age_m
#               + factor(uf)
#               , data = df_est)


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



m = feglm(log(emprego_lths) ~ sunab(semestre_entrada_did, anosem_did)
          | id_municipio + anosem_did ,
          data = df,
          weights = ~peso,
          cluster = 'rgi')
iplot(m)


