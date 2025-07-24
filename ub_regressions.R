# install_github("bcallaway11/BMisc", dependencies = TRUE)
# install_github("asheshrambachan/HonestDiD", dependencies = TRUE)

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
dropar_primeiros_munics = 0
minimo_cidades = 10
maximo_periodo = 20202
# minimo_habs = 50000

#definir controles
controles = as.formula(" ~ mean_income_m+ unem_rate_m + inf_rate_m + lpop +lpop2")
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
                    # output = 'latex',
                    stars = TRUE)


tab = df_sum[tem_uber == 1, (count = .N), by = semestre_entrada]
tab[, `Semestre Entrada` := as.integer(semestre_entrada)]
tab[, `N Municípios` := as.integer(V1)]
tab[, `:=`(semestre_entrada = NULL, V1 = NULL)]
tab = tab %>% arrange(`Semestre Entrada`)
datasummary_df(tab, 
               # output = "latex_tabular",
               fmt = 0)

#salários por tipo de emprego em 2014.
wages_emp = data.table(
  overall = mean(df[anosem == 20142, .(salario_privado)], na.rm = TRUE),
  temporario = mean(df[anosem == 20142, .(salario_temporario)], na.rm = TRUE),
  meio_periodo = mean(df[anosem == 20142, .(salario_meio_periodo)], na.rm = TRUE)
)

######################################
#Emprego privado
######################################
set.seed(456)
m2 = regressao_cs(variavel_dependente = "emprego_lths",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p2 = plot_es(m2, title = " ")
p2 %>% print()

#save original overall att to use later
or_att_emp = m2[[2]]$overall.att

ggsave("../Output/202504/emprego_noeduc.png", height = 5, width = 9)

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
plot_grid(p1,p2, nrow = 1)

#save original overall att for lths to use later
or_att_emp_lths = m1[[2]]$overall.att

ggsave("../Output/202504/emprego_privado_escolaridade.png", height = 5, width = 9)

######################################
# Emprego temporário e Meio Período
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_temporario",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_meio_periodo",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = 'Log Temporary Employment')
p2 = plot_es(m2, title = "Log Part-Time Employment")
plot_grid(p1,p2, nrow = 1)

ggsave("../Output/202504/empregos_alternativos.png")


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

p1 = plot_es(m1, title = 'Less Than High School')
p2 = plot_es(m2, title = "High School or More")
plot_grid(p1,p2, nrow = 1)

ggsave("../Output/202504/emprego_temporario_educ.png", height = 5, width = 9)

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
  
  
  p1 = plot_es(m1, title = "Less Than High School")
  p2 = plot_es(m2, title = 'High School or More')
  plot_grid(p1,p2, nrow = 1)


ggsave("../Output/202504/emprego_meio_periodo_educ.png", height = 5, width = 9)


######################################
# Log Salarios
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "salario_privado",
                  dep_em_log = 1,
                  controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = ' ')+theme_minimal()
p1 %>% print()
ggsave("../Output/202504/wages.png", height = 4.5, width = 8)

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


p1 = plot_es(m1, title = "Less Than High School")
p2 = plot_es(m2, title = 'High School or More')
plot_grid(p1,p2, nrow = 1)

#save original overall att to use later
or_att_wage_hs = m2[[2]]$overall.att

ggsave("../Output/202504/wages_educ.png", height = 5, width = 9)


######################################
# Emprego Publico E Rural
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_publico",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_rural",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "salario_publico",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m4 = regressao_cs(variavel_dependente = "salario_rural",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = 'Public Employment')
p2 = plot_es(m2, title = "Rural Employment")
p3 = plot_es(m3, title = "Public Wages")
p4 = plot_es(m4, title = "Rural Wages")
plot_grid(p1,p2, p3, p4, nrow = 2)

ggsave("../Output/202504/emprego_salario_publico.png", height = 9, width = 9)

######################################
# Salário Publico e Rural
######################################
set.seed(456)







# ######################################
# # Log Salarios - Tipo de Emprego
# ######################################
# set.seed(456)
# m1 = regressao_cs(variavel_dependente = "salario_temporario",
#                   dep_em_log= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# m2 = regressao_cs(variavel_dependente = "salario_meio_periodo",
#                   dep_em_log= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# 
# p1 = plot_es(m1, title = "Temporary")
# p2 = plot_es(m2, title = 'Half Time')
# pg = plot_grid(p1,p2, nrow = 1)
# title = ggdraw() + draw_label("Log Wages", 
#                               fontface='bold')
# plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))
# 
# ggsave("../Output/202504/wages_type_emp.png")




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

ggsave("../Output/202504/emprego_privado_escolaridade_universal.png", height = 5, width = 9)


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

ggsave("../Output/202504/wages_educ_universal.png", height = 9, width = 9)





######################################
# Homicidios e Acidentes de transito
######################################
m1 = regressao_cs(variavel_dependente = "homicidios_pc",
                  controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "mortes_acidente_carro_pc",
                  controles_use = controles, dep_em_log =1,
                  control_group = "notyettreated")
p1 = plot_es(m1, title = 'Homicidios/100k Hab')
p2 = plot_es(m2, title = "Mortes Acidente de Carros/ 100k Hab")
pg = plot_grid(p1,p2, nrow = 1)
title = ggdraw() + draw_label("Mortes - SIM-SUS", fontface='bold')
plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))

ggsave("../Output/202504/homicidios_acidentes.png", height = 4.5, width = 8)


######################################
#Tentativa 2WFE
######################################
# source("../uber2/ub_funcoes_auxiliares.R")
# fe = regressao_fe(variavel_dependente = "emprego_hs", dep_em_log = 1)
# summary(fe)
# plot_fe(fe)
# 
# p2 = plot_es(m2, title = "Log Formal Private Employment")
# p2 %>% print()

######################################
#Tentativa com Leave-One-Out
######################################
set.seed(456)
grupos = df %>% 
  filter(!is.na(semestre_entrada)) %>% 
  arrange(semestre_entrada) %>% 
  select(semestre_entrada) %>% unique() %>% pull()

df_back = copy(df)
tabela_resultados = data.table()

for(grupo in grupos){
  df = df_back[!semestre_entrada %in% grupo]
  m1 = regressao_cs(variavel_dependente = "emprego_privado",
                    dep_em_log= 1, controles_use = controles, 
                    control_group = "notyettreated")
  
  m2 = regressao_cs(variavel_dependente = "emprego_lths",
                    dep_em_log= 1, controles_use = controles, 
                    control_group = "notyettreated")
  
  m3 = regressao_cs(variavel_dependente = "salario_hs",
                    dep_em_log= 1, controles_use = controles, 
                    control_group = "notyettreated")
  semestre = as.character(grupo)
  #pegar dados do primeiro modelo
  att1 = m1[[2]]$overall.att
  inf1 = (-1.96*m1[[2]]$overall.se)+att1
  sup1 = (1.96*m1[[2]]$overall.se)+att1
  
  #pegar dados do segundo modelo
  att2 = m2[[2]]$overall.att
  inf2 = (-1.96*m2[[2]]$overall.se)+att2
  sup2 = (1.96*m2[[2]]$overall.se)+att2
  
  #pegar dados do terceiro modelo
  att3 = m3[[2]]$overall.att
  inf3 = (-1.96*m3[[2]]$overall.se)+att3
  sup3 = (1.96*m3[[2]]$overall.se)+att3
  
  new_line = data.table(semestre, att1, inf1, sup1,
                        att2, inf2, sup2, 
                        att3, inf3, sup3)
  tabela_resultados = rbind(tabela_resultados, new_line)
  df = copy(df_back)
}

aux_theme = theme_classic() +
  theme(plot.title = element_text(color="darkgray", face="bold", size=9),
        axis.title = element_text(color="darkgray", face="bold", size=9),
        strip.background = element_rect(fill = 'white', color = 'white'),
        strip.text = element_text(color = 'darkgray', face = 'bold', size = 9, hjust = 0),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


p1 = ggplot(tabela_resultados, aes(x = semestre, y = att1))+
  geom_point()+
  geom_errorbar(aes(ymin = inf1, ymax = sup1), width = 0.1)+
  labs(x = ' ', y = 'Overall ATT', title = "Formal Emp.")+
  geom_hline(aes(yintercept =0))+
  geom_hline(aes(yintercept = or_att_emp), linetype = "dotted")+aux_theme

p2 = ggplot(tabela_resultados, aes(x = semestre, y = att2))+
  geom_point()+
  geom_errorbar(aes(ymin = inf2, ymax = sup2), width = 0.1)+
  labs(x = ' ', y = ' ', title = "Formal Emp. - LTHS")+
  geom_hline(aes(yintercept =0))+
  geom_hline(aes(yintercept = or_att_emp_lths), linetype = "dotted")+aux_theme

p3 = ggplot(tabela_resultados, aes(x = semestre, y = att3))+
  geom_point()+
  geom_errorbar(aes(ymin = inf3, ymax = sup3), width = 0.1)+
  labs(x = ' ', y = ' ', title = "Wages - HS or More")+
  geom_hline(aes(yintercept =0))+
  geom_hline(aes(yintercept = or_att_wage_hs), linetype = "dotted")+aux_theme

plot_grid(p1, p2, p3, nrow = 1)
ggsave("../Output/202504/leave_one_out.png", height = 4.5, width = 8)


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



######################################
#Teste
######################################
set.seed(456)
m2 = regressao_cs(variavel_dependente = "salario_hs_cbo",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p2 = plot_es(m2, title = " ")
p2 %>% print()

######################################
#Simulations with random treatment dates
######################################
min_did_date = min(df[semestre_entrada_did!=0, .(semestre_entrada_did)])
max_did_date = max(df[semestre_entrada_did!=0, .(semestre_entrada_did)])
possible_did_dates = seq(min_did_date, max_did_date,1)

#backup original entry dates
df[, original_did_dates := semestre_entrada_did]
fake_att = data.table()
for(i in c(1:1000)){
  
  #input random entry dates for treated municipalities, but
  #guarantee a minimum of 10 cities per treated group
  min_cities = 0
  while(min_cities < 10){
    aux = df[anosem == 20151, .(id_municipio, semestre_entrada_did)] #just taking one obs per munic
    aux[, semestre_entrada_fake := sample(possible_did_dates, nrow(aux), replace = TRUE)]
    aux[, semestre_entrada_fake := fifelse(semestre_entrada_did == 0, 
                                           0, semestre_entrada_fake)]
    min_cities = count(aux, semestre_entrada_fake) %>% select(n) %>% min()
    
  }
  aux = aux[, .(id_municipio, semestre_entrada_fake)]
  df = merge(df, aux, by = "id_municipio", all.x = TRUE)
  df[, semestre_entrada_did := semestre_entrada_fake]
  df[, semestre_entrada_fake := NULL]
  
  #run models
  emp = regressao_cs(variavel_dependente = "emprego_privado",
                     dep_em_log= 1, controles_use = controles, 
                     control_group = "notyettreated")
  
  emp_lths = regressao_cs(variavel_dependente = "emprego_lths",
                          dep_em_log= 1, controles_use = controles, 
                          control_group = "notyettreated")
  
  wage_hs = regressao_cs(variavel_dependente = "salario_hs",
                         dep_em_log= 1, controles_use = controles, 
                         control_group = "notyettreated")
  
  new_line = data.table(
    att_emp = emp[[2]]$overall.att,
    att_emp_lths = emp_lths[[2]]$overall.att,
    wage_hs = wage_hs[[2]]$overall.att
  )
  
  #save on table
  fake_att = rbind(fake_att, new_line)
  
  #recover original dates
  df[, semestre_entrada_did := original_did_dates]
  
  if((i %% 100) == 0) print(paste0(i, "% complete"))
}


#plot for overall employment
ggplot(fake_att, aes(att_emp))+geom_histogram()+
  geom_vline(aes(xintercept = or_att_emp))+ theme_minimal()+
  labs(x = 'Fake ATTs', y = 'Count')+aux_theme

#plot for lths employment
ggplot(fake_att, aes(att_emp_lths))+geom_histogram()+
  geom_vline(aes(xintercept = or_att_emp_lths))+ theme_minimal()+
  labs(x = 'Fake ATTs', y = 'Count')+aux_theme

#plot for hs wages
ggplot(fake_att, aes(wage_hs))+geom_histogram()+
  geom_vline(aes(xintercept = or_att_wage_hs))+ theme_minimal()+
  labs(x = 'Fake ATTs', y = 'Count')+aux_theme




