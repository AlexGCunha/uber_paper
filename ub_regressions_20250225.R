library(devtools)
# install_github("bcallaway11/BMisc", dependencies = TRUE)
# install_github("asheshrambachan/HonestDiD", dependencies = TRUE)


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
dropar_primeiros_munics = 0
minimo_cidades = 10
maximo_periodo = 20202
minimo_habs = 50000

#definir controles
controles = as.formula(" ~ mean_income_m+ unem_rate_m + inf_rate_m + lths_rate_m  + hs_rate_m ")
sem_controles = as.formula("~ 1")

######################################
#Ajustes
######################################
#Dropar primeiros municípios e ultimos municipios
if(dropar_primeiros_munics == 1){
  df = df[!(tem_uber == 1 & semestre_entrada %in% c(20141, 20142, 20151, 20152))]
}

#dropar grupos onde o numero de cidades tratadas no periodo é menor que o minimo
df[, conta_cidade_grupo := length(unique(id_municipio)), by = .(semestre_entrada_did)]
df = df[conta_cidade_grupo >=minimo_cidades]

#Dropar minimo de habitantes
df = df[pop14 >= minimo_habs]

source("../uber2/ub_funcoes_auxiliares.R")
dia = "202502"

######################################
#Summary Statistics
######################################
df_sum = copy(df)
df_sum = df_sum[anosem == 20142]
df_sum[,`:=`(unem_rate_m = unem_rate_m * 100,
            inf_rate_m = inf_rate_m *100,
            lths_rate_m = lths_rate_m * 100,
            hs_rate_m = hs_rate_m * 100)]
df_sum[, N := .N, by =.(tem_uber)]

setnames(df_sum, old = c('pop14' , 'pibpc14' , 'mean_income_m' , 'unem_rate_m'
                         , 'inf_rate_m'  , 'lths_rate_m' , 'hs_rate_m'
                         , 'emprego_privado', 'salario_privado' , 'tem_uber'),
         new = c('Population' , 'GDP per cap (2014)' , 'Mean Income' , 'Unemployment Rate'
                       , 'Informality Rate'  , 'Share Less than High School' , 'Share High School'
                       , 'Private Employment', 'Private Wages' , 'tem_uber'))

datasummary_balance(`Population` + `GDP per cap (2014)` + `Mean Income` + `Unemployment Rate`
                    + `Informality Rate`  + `Share Less than High School`  + `Share High School` + `Private Employment` 
                    + `Private Wages` + `N`~ tem_uber,
                    data = df_sum,
                    output = 'latex',
                    stars = TRUE)
                    

tab = df_sum[tem_uber == 1, (count = .N), by = semestre_entrada]
tab[, `Semestre Entrada` := as.integer(semestre_entrada)]
tab[, `N Municípios` := as.integer(V1)]
tab[, `:=`(semestre_entrada = NULL, V1 = NULL)]
tab = tab %>% arrange(`Semestre Entrada`)
datasummary_df(tab, fmt = 0, output = "latex_tabular")

######################################
#Emprego privado
######################################
set.seed(456)
m2 = regressao_cs(variavel_dependente = "emprego_privado",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p2 = plot_es(m2, title = "Log Formal Private Employment")
p2 %>% print()

ggsave("../Output/202502/emprego_noeduc.png")

######################################
# Emprego privado- Por Educação
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_lths",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_hs",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = 'Less Than HS')
p2 = plot_es(m2, title = "HS or more")
pg = plot_grid(p1,p2, nrow = 1)
title = ggdraw() + 
  draw_label("Log Formal Private Employment", 
             fontface='bold')
plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))

ggsave("../Output/202502/emprego_privado_escolaridade.png")


# ######################################
# # Emprego privado- Por salario
# ######################################
# m1 = regressao_cs(variavel_dependente = "emprego_baixo_sal",
#                   dep_em_log= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m2 = regressao_cs(variavel_dependente = "emprego_alto_sal",
#                   dep_em_log= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# p1 = plot_es(m1, title = 'Low Wages (1.5 MS 2010)')
# p2 = plot_es(m2, title = "High Wages (1.5 MS 2010)")
# pg = plot_grid(p1,p2, nrow = 1)
# title = ggdraw() + 
#   draw_label("Log Formal Private Employment", 
#              fontface='bold')
# plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))
# 
# ggsave("../Output/202502/emprego_privado_wage.png")


######################################
# Emprego temporário
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_temporario",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_n_temporario",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = 'Log Temporary Employment')
p2 = plot_es(m2, title = "Log Non-Temporary Employment")
pg = plot_grid(p1,p2, nrow = 1)
title = ggdraw() + draw_label("Log Temporary Employment", 
                              fontface='bold')
plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))

ggsave("../Output/202502/emprego_temporario.png")


######################################
# Emprego temporário - por educação
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_temporario_lths",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_temporario_hs",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = 'Log Temporary Employment - LTHS')
p2 = plot_es(m2, title = "Log Temporary Employment - HS")
pg = plot_grid(p1,p2, nrow = 1)
title = ggdraw() + draw_label("Log Temporary Employment", 
                              fontface='bold')
plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))

ggsave("../Output/202502/emprego_temporario_educ.png")

######################################
# Emprego Meio Período
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_meio_periodo",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")
m2 = regressao_cs(variavel_dependente = "emprego_privado_full",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = "Half Time Employment (<= 20h)")
p2 = plot_es(m2, title = 'Full Time Employment (>= 40h)')
pg = plot_grid(p1,p2, nrow = 1)
title = ggdraw() + draw_label("Log Half Time Employment", 
                              fontface='bold')
plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))

ggsave("../Output/202502/emprego_full_time.png")


######################################
# Emprego Meio Período - Por educação
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_meio_periodo_lths",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")
m2 = regressao_cs(variavel_dependente = "emprego_meio_periodo_hs",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = "Half Time - LTHS")
p2 = plot_es(m2, title = 'Half Time - HS or More')
pg = plot_grid(p1,p2, nrow = 1)
title = ggdraw() + draw_label("Log Half Time Employment", 
                              fontface='bold')
plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))

ggsave("../Output/202502/emprego_meio_periodo_educ.png")


######################################
# Log Salarios
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "salario_privado",
                  dep_em_log = 1,
                  controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = 'Log Wages')
p1 %>% print()
ggsave("../Output/202502/wages.png")

######################################
# Log Salarios - Educ
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "salario_lths",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")
m2 = regressao_cs(variavel_dependente = "salario_hs",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = "LTHS")
p2 = plot_es(m2, title = 'HS or More')
pg = plot_grid(p1,p2, nrow = 1)
title = ggdraw() + draw_label("Log Wages", 
                              fontface='bold')
plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))

ggsave("../Output/202502/wages_educ.png")


######################################
# Log Salarios - Tipo de Emprego
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "salario_temporario",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")
m2 = regressao_cs(variavel_dependente = "salario_meio_periodo",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = "Temporary")
p2 = plot_es(m2, title = 'Half Time')
pg = plot_grid(p1,p2, nrow = 1)
title = ggdraw() + draw_label("Log Wages", 
                              fontface='bold')
plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))

ggsave("../Output/202502/wages_type_emp.png")



######################################
# Emprego Publico
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_publico",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_rural",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = 'Public Employment')
p2 = plot_es(m2, title = "Rural Employment")
pg = plot_grid(p1,p2, nrow = 1)
title = ggdraw() + draw_label("Log Other Employment", 
                              fontface='bold')
plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))

ggsave("../Output/202502/emprego_publico.png")

######################################
# Emprego privado- Por Educação - BASE UNIVERSAL
######################################
m1 = regressao_cs(variavel_dependente = "emprego_lths",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")

m2 = regressao_cs(variavel_dependente = "emprego_hs",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")

p1 = plot_es(m1, title = 'Less Than HS')
p2 = plot_es(m2, title = "HS or more")
pg = plot_grid(p1,p2, nrow = 1)
title = ggdraw() + 
  draw_label("Log Formal Private Employment", 
             fontface='bold')
plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))

ggsave("../Output/202502/emprego_privado_escolaridade_universal.png")


######################################
# Log Salarios - Educ - base universal
######################################
m1 = regressao_cs(variavel_dependente = "salario_lths",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")
m2 = regressao_cs(variavel_dependente = "salario_hs",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")


p1 = plot_es(m1, title = "LTHS")
p2 = plot_es(m2, title = 'HS or More')
pg = plot_grid(p1,p2, nrow = 1)
title = ggdraw() + draw_label("Log Wages", 
                              fontface='bold')
plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))

ggsave("../Output/202502/wages_educ_universal.png")





######################################
# Homicidios e Acidentes de transito
######################################
m1 = regressao_cs(variavel_dependente = "homicidios_pc",
                  controles_use = controles_noeduc, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "mortes_acidente_carro_pc",
                  controles_use = controles_noeduc, dep_em_log =1,
                  control_group = "notyettreated")
p1 = plot_es(m1, title = 'Homicidios/100k Hab')
p2 = plot_es(m2, title = "Mortes Acidente de Carros/ 100k Hab")
pg = plot_grid(p1,p2, nrow = 1)
title = ggdraw() + draw_label("Mortes - SIM-SUS", fontface='bold')
plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))

ggsave("../Output/202502/homicidios_acidentes.png")


######################################
#Tentativa 2WFE
######################################
source("../uber2/ub_funcoes_auxiliares.R")
fe = regressao_fe(variavel_dependente = "emprego_hs", dep_em_log = 1)
summary(fe)
plot_fe(fe)

p2 = plot_es(m2, title = "Log Formal Private Employment")
p2 %>% print()

######################################
#Leave one out grupo
######################################
grupos = df %>% 
  filter(!is.na(semestre_entrada)) %>% 
  arrange(semestre_entrada) %>% 
  select(semestre_entrada) %>% unique() %>% pull()

df_back = copy(df)
tabela_resultados = data.table()

for(grupo in grupos){
  df = df_back[!semestre_entrada %in% grupo]
  modelo = regressao_cs(variavel_dependente = "salario_hs",
                        dep_em_log= 1, controles_use = controles, 
                        control_group = "notyettreated")
  semestre = as.character(grupo)
  att = modelo[[2]]$overall.att
  inf = (-1.96*modelo[[2]]$overall.se)+att
  sup = (1.96*modelo[[2]]$overall.se)+att
  new_line = data.table(semestre, att, inf, sup)
  tabela_resultados = rbind(tabela_resultados, new_line)
  df = copy(df_back)
}


ggplot(tabela_resultados, aes(x = semestre, y = att))+
  geom_point()+
  geom_errorbar(aes(ymin = inf, ymax = sup))


######################################
#Sensitivity Analysis 
######################################

#Emprego privado por educação
m1 = regressao_cs(variavel_dependente = "emprego_lths",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")

m2 = regressao_cs(variavel_dependente = "emprego_hs",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")
hd_m1_relative = honest_did(es = m1[[2]], e = 5, type = "relative_magnitude")
createSensitivityPlot_relativeMagnitudes(hd_m1_relative$robust_ci,
                                         hd_m1_relative$orig_ci)

#Salários por educação
m1 = regressao_cs(variavel_dependente = "salario_lths",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")
m2 = regressao_cs(variavel_dependente = "salario_hs",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")
hd_m2_relative = honest_did(es = m2[[2]], e = 3, type = "relative_magnitude")
createSensitivityPlot_relativeMagnitudes(hd_m2_relative$robust_ci,
                                         hd_m2_relative$orig_ci)


