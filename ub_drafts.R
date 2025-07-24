#Comparar distribuição da população entre cidades tratadas e não tratadas

#abrir dataset
df = read_parquet("../data/ub_rais_merged.parquet")
gross = df[anosem == 20142]
gross[, tratado := as.character(fifelse(is.na(semestre_entrada), 0, 1))]
ggplot(gross, aes(lpop, colour = tratado, group = tratado))+geom_density()+
  labs(title = 'Distribution of Log Pop among treatement groups')

#filtrar municípios com população similar
pop_similar = gross[lpop >= 9.5 & lpop <= 12]
count(pop_similar, tratado)
min(pop_similar$pop14)
max(pop_similar$pop14)













#######################################
#Check dos dados
######################################


#abrir dataset
df = read_parquet("../data/ub_rais_merged.parquet")

#parametros:
#dropar primeiros municipios
dropar_primeiros_munics = 0
minimo_cidades = 5
maximo_periodo = 20202
minimo_habs = 13378
maximo_habs = 162379

#definir controles
controles = as.formula(" ~ mean_income_m+ unem_rate_m + inf_rate_m + lths_rate_m  + hs_rate_m ")
sem_controles = as.formula("~ 1")

######################################
#Ajustes
######################################
#Dropar minimo de habitantes
df = df[pop14 >= minimo_habs]
df = df[pop14 <= maximo_habs]

#dropar grupos onde o numero de cidades tratadas no periodo é menor que o minimo
df[, conta_cidade_grupo := length(unique(id_municipio)), by = .(semestre_entrada_did)]
df = df[conta_cidade_grupo >=minimo_cidades]



source("../uber2/ub_funcoes_auxiliares.R")
dia = "202504"

######################################
#Summary Statistics
######################################
df_sum = copy(df)
df_sum = df_sum[anosem == 20142]
tab = df_sum[tem_uber == 1, (count = .N), by = semestre_entrada]
tab[, `Semestre Entrada` := as.integer(semestre_entrada)]
tab[, `N Municípios` := as.integer(V1)]
tab[, `:=`(semestre_entrada = NULL, V1 = NULL)]
tab = tab %>% arrange(`Semestre Entrada`)
datasummary_df(tab, 
               # output = "latex_tabular",
               fmt = 0)
print(sum(tab$`N Municípios`))

######################################
#Emprego privado
######################################
set.seed(456)
m2 = regressao_cs(variavel_dependente = "emprego_privado",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p2 = plot_es(m2, title = " ")
p2 %>% print()









