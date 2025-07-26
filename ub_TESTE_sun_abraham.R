


######################################
#Tentativa Sun and Abraham com pesos
######################################
library(fixest)
df = read_parquet("../data/ub_rais_merged.parquet")
df[, uf := substr(id_municipio, 1, 2)]
df[, region := substr(id_municipio, 1,1)]
df[, tratado := fifelse(is.na(semestre_entrada), 0, 1)]
df[, pop2 := pop14^2]
df = df[!(tem_uber == 1 & semestre_entrada %in% c(20141, 20142, 20151, 20152))]
df_est = df[anosem == 20142]



#Estimar propensity score
ps_model = glm(tratado ~  log(mean_income_m)
               + unem_rate_m 
               + inf_rate_m #+ lths_rate_m  + hs_rate_m
               + log(pibpc14) #+ I(log(pibpc14)^2)
               + log(employed_m) 
               # + pibpc14 + I(pibpc14^2)
               # + log(pea_m) #+ I(log(pea_m^2))
               + age_m 
               + log(pop_m) 
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



pesos = df$peso

########################
############sunab
########################
m = feglm(log(emprego_lths) ~ sunab(semestre_entrada_did, anosem_did) +lpop_t
          | id_municipio + anosem_did ,
          data = df,
          weights = pesos,
          cluster = 'id_municipio')
iplot(m)


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
                        outcome = 'lemprego_lths',
                        group = 'id_municipio', 
                        time = 'anosem_did',
                        treatment = 'tratado',
                        effects = 6, placebo = 6, cluster = 'id_municipio',
                        controls = c('lpop_t'),
                        weight =  'peso')


