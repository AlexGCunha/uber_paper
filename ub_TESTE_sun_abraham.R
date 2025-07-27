


######################################
#Tentativa Sun and Abraham com pesos
######################################
library(fixest)
df = read_parquet("../data/ub_rais_merged.parquet")
df = df[anosem <= 20192]
df = df[!(semestre_entrada %in% c(20141, 20142, 20151, 20152))]
df[, uf := substr(id_municipio, 1, 2)]
df[, region := substr(id_municipio, 1,1)]
df[, tratado := fifelse(is.na(semestre_entrada), 0, 1)]
df[, pop2 := pop14^2]
df = df[ pop_m >= 50000]
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
               # + lemployed_r
               # + lpop
               # + lmean_pop_r
               
               + age_r
               + factor(region)
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


m = feols(log(emprego_privado) ~ sunab(semestre_entrada_did, anosem_did) + lpop_r_t
          | id_municipio + anosem_did + uf ,
          data = df,
          weights = ~peso,
          cluster = 'rgi')

iplot(m)


