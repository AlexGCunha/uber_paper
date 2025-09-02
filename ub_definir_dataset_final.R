################################################################################
#Esse código irá:
#- Selecionar a amostra final de regiões
################################################################################
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
library(readxl)



#abrir dataset
df = read_parquet("../data/ub_rais_merged.parquet")
source("../uber/ub_funcoes_auxiliares.R")

#parametros:
dropar_primeiros_munics = 0
minimo_cidades = 9

#manter dados até 2019
df = df[anosem <= 20192]

######################################
#Propensity Score
######################################
df[,uf := substr(rgi,1, 2)]
df[,region := substr(rgi, 1,1)]
df_est = df[anosem == 20142]

ps_model = glm(tratado ~  
                 lincome_r
               + unem_rate_r
               + lemprego_14
               + lpea_r
               , data = df_est,
               family = 'binomial')


print(summary(ps_model))
df_est[, prob := predict(ps_model, type = 'response')]
df_est[, tratado := as.character(tratado)]

#calcular probs min e maxima por tratamento
prob_min_tratado = min(df_est[tratado==1]$prob)
prob_min_controle = min(df_est[tratado==0]$prob)
prob_max_tratado = max(df_est[tratado==1]$prob)
prob_max_controle = max(df_est[tratado==0]$prob)


#Adicionar ao df principal
df_est = df_est[, .(rgi, prob)]
df = merge(df, df_est, by = 'rgi', all.x = TRUE)

#limpar probs muito altas ou muito baixas
df = df[prob >= 0.01 & prob <= 0.99]
# df = df[prob >= 
#           max(prob_min_tratado, prob_min_controle) & prob <= min(prob_max_tratado, prob_max_controle)]

#dropar grupos onde o numero de cidades tratadas no periodo é menor que o minimo
df[, conta_cidade_grupo := length(unique(rgi)), by = .(semestre_entrada_did)]
df = df[conta_cidade_grupo >=minimo_cidades]


#Grafico da distribuicao de população e Propensity Scores por tratamento
df_plot = df[anosem == 20142]
df_plot[, tratado := as.character(tratado)]
p1 = ggplot(df_plot, aes(prob, color = tratado, group = tratado))+geom_density()+
  labs(title = 'Distribuicao Prop. Score por tratamento')+theme(legend.position = 'bottom')

p2 = ggplot(df_plot, aes(lpea_r, color = tratado, group = tratado))+geom_density()+
  labs(title = 'Distribuicao Log PEA por tratamento')+theme(legend.position = 'bottom')

plot_grid(p2, p1)
# ggsave(paste0(path_save,"distribuicoes_prob_pop.png"), height = 5, width = 9)


######################################
#Summary Statistics
######################################
df_sum = copy(df)
df_sum = df_sum[anosem == 20142]
df_sum[,`:=`(unem_rate_r = unem_rate_r * 100,
             inf_rate_r = inf_rate_r *100,
             lths_rate_r = lths_rate_r * 100,
             hs_rate_r = hs_rate_r * 100)]
df_sum[, N := .N, by =.(tratado)]

setnames(df_sum, old = c('pea_r'  , 'mean_income_r' , 'unem_rate_r'
                         , 'inf_rate_r'  , 'lths_rate_r' , 'hs_rate_r'
                         , 'emprego_privado', 'salario_privado' , 'tratado'),
         new = c('Labor Force'  , 'Mean Income' , 'Unemployment Rate'
                 , 'Informality Rate'  , 'Share Less than High School' , 'Share High School'
                 , 'Private Employment', 'Private Wages' , 'tem_uber'))

datasummary_balance(`Labor Force`  + `Mean Income` + `Unemployment Rate`
                    + `Informality Rate`  + `Share Less than High School`  + `Share High School` + `Private Employment` 
                    + `Private Wages` + `N`~ tem_uber,
                    data = df_sum,
                    # output = 'latex',
                    stars = TRUE)


tab = df_sum[, (count = .N), by = semestre_entrada]
tab[, `Semestre Entrada` := as.integer(semestre_entrada)]
tab[, `N Municípios` := as.integer(V1)]
tab[, `:=`(semestre_entrada = NULL, V1 = NULL)]
tab = tab %>% arrange(`Semestre Entrada`)
datasummary_df(tab, 
               # output = "latex_tabular",
               fmt = 0)

#salvar
write_parquet(df, '../data/ub_final_dataset.parquet')
rm(list =ls())
gc()
