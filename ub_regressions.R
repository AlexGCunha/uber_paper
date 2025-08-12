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
source("../uber/ub_funcoes_auxiliares.R")

#parametros:
dropar_primeiros_munics = 0
minimo_cidades = 9

#manter dados até 2019
df = df[anosem <= 20192]

#definir controles
controles = as.formula(" ~  lincome_r  + unem_rate_r  +inf_rate_r +lpea_r")
sem_controles = as.formula("~ 1")

#definir pasta
data = "202507"
path_save = paste0("../Output/",data,"/")



######################################
#Ajustes
######################################
#Dropar primeiros municípios e ultimos municipios
if(dropar_primeiros_munics == 1){
  df = df[!(semestre_entrada %in% c(20141, 20142, 20151, 20152))]
}

df[,uf := substr(rgi,1, 2)]
df[,region := substr(rgi, 1,1)]
df_est = df[anosem == 20142]

######################################
#Propensity Score
######################################
#Estimar propensity score
ps_model = glm(tratado ~  lincome_r
               + lpea_r
               + unem_rate_r
               + inf_rate_r
               # + lemployed_r
               # + factor(region)
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
ggsave(paste0(path_save,"distribuicoes_prob_pop.png"), height = 5, width = 9)


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



######################################
#Emprego privado
######################################
source("../uber/ub_funcoes_auxiliares.R")
set.seed(456)
m2 = regressao_cs(variavel_dependente = "emprego_privado",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p2 = plot_es(m2, title = " ")
p2 %>% print()
or_att_emp = m2[[2]]$overall.att

ggsave(paste0(path_save,"emprego_noeduc.png"), height = 5, width = 9)

######################################
# Emprego privado- Por Educação
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_lths",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_hs_somecol",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_col",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = 'Less Than HS')+ylim(-0.15, 0.07)
p2 = plot_es(m2, title = "HS/ Some Coll.")+ylim(-0.15, 0.07)
p3 = plot_es(m3, title = "College or More")+ylim(-0.15, 0.07)
plot_grid(p1,p2, p3, nrow = 1)

#save original overall att for lths to use later
or_att_emp_lths = m1[[2]]$overall.att

ggsave(paste0(path_save,"emprego_privado_escolaridade.png"), height = 5, width = 9)


######################################
# Emprego privado- Por Salario
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_baixo_sal",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_med_sal",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_alto_sal",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m4 = regressao_cs(variavel_dependente = "emprego_altissimo_sal",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = 'Log Emp: (,1500]')+ylim(-0.2, 0.1)
p2 = plot_es(m2, title = "Log Emp: (1500,3000]")+ylim(-0.2, 0.1)
p3 = plot_es(m3, title = "Log Emp: [3000, 6000]")+ylim(-0.2, 0.1)
p4 = plot_es(m4, title = "Log Emp: (6000,)")+ylim(-0.2, 0.1)
plot_grid(p1,p2, p3, p4, nrow = 2)

ggsave(paste0(path_save,"emprego_privado_nivel_sal.png"), height = 5, width = 9)

######################################
# Emprego privado- Por Sexo
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_homens",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_mulheres",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = 'Log Employment - Men')+ylim(-0.15, 0.07)
p2 = plot_es(m2, title = "Log Employment - Women")+ylim(-0.15, 0.07)
plot_grid(p1,p2, nrow = 1)

ggsave(paste0(path_save,"emprego_privado_sexo.png"), height = 5, width = 9)

######################################
# Emprego privado- Por Idade
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_baixo_idade",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_med_idade",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_alto_idade",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = 'L Emp: (,30]')+ylim(-0.10, 0.05)
p2 = plot_es(m2, title = "L Emp: (30,45]")+ylim(-0.1, 0.05)
p3 = plot_es(m3, title = "L Emp: (45,)")+ylim(-0.10, 0.05)
plot_grid(p1,p2, p3, nrow = 1)

ggsave(paste0(path_save,"emprego_privado_idade.png"), height = 5, width = 9)


######################################
# Emprego privado- Por Rank de Salário CBO
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_baixo_cbo",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_med_cbo",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_alto_cbo",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m4 = regressao_cs(variavel_dependente = "emprego_altissimo_cbo",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = 'Log Emp: 1st wage quart.')+ylim(-0.15, 0.05)
p2 = plot_es(m2, title = "Log Emp: 2nd wage quart.")+ylim(-0.15, 0.05)
p3 = plot_es(m3, title = "Log Emp: 3rd wage quart.")+ylim(-0.15, 0.05)
p4 = plot_es(m4, title = "Log Emp: 4th wage quart.")+ylim(-0.15, 0.05)
plot_grid(p1,p2, p3, p4, nrow = 2)

ggsave(paste0(path_save,"emprego_privado_rank_sal_cbo.png"), height = 5, width = 9)


######################################
# Emprego privado- Por investimento em RD (OCDE)
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_hintensity",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_lintensity",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")


p2 = plot_es(m1, title = 'Log Emp: High Research Sectors')+ylim(-0.20, 0.1)
p1 = plot_es(m2, title = "Log Emp: Low Research Sectors.")+ylim(-0.20, 0.1)
plot_grid(p1,p2)

ggsave(paste0(path_save,"emprego_inv_pesquisa.png"), height = 5, width = 9)


######################################
# Emprego privado- Por quantil de share com college
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_lcol",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_mcol",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_hcol",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = 'Log Emp: Low College Sect.')+ylim(-0.15, 0.1)
p2 = plot_es(m2, title = "Log Emp: Mid College Sect.")+ylim(-0.15, 0.1)
p3 = plot_es(m3, title = "Log Emp: High College Sect.")+ylim(-0.15, 0.1)
plot_grid(p1,p2, p3, nrow = 1)

ggsave(paste0(path_save,"emprego_share_col.png"), height = 5, width = 9)

######################################
# Emprego temporário e Meio Período
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_meio_periodo",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = "Log Part-Time Employment")
print(p1)


ggsave(paste0(path_save,"empregos_alternativos.png"), height = 5, width = 9)


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
ggsave(paste0(path_save,"wages.png"), height = 4.5, width = 8)

######################################
# Log Salarios - Educ
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "salario_lths",
                  dep_em_log= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")
m2 = regressao_cs(variavel_dependente = "salario_hs_somecol",
                  dep_em_log= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "salario_col",
                  dep_em_log= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")



p1 = plot_es(m1, title = "Less Than High School")+ylim(-0.08, 0.04)
p2 = plot_es(m2, title = 'HS/ Some Coll.')+ylim(-0.08, 0.04)
p3 = plot_es(m3, title = 'College')+ylim(-0.08, 0.04)
plot_grid(p1,p2, p3, nrow = 1)

ggsave(paste0(path_save,"wages_educ.png"), height = 5, width = 9)

######################################
# Log Salario- Por Rank de Salário CBO
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "salario_baixo_cbo",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "salario_med_cbo",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "salario_alto_cbo",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m4 = regressao_cs(variavel_dependente = "salario_altissimo_cbo",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = 'Log Wage: 1st wage quart.')+ylim(-0.1, 0.05)
p2 = plot_es(m2, title = "Log Wage: 2nd wage quart.")+ylim(-0.1, 0.05)
p3 = plot_es(m3, title = "Log Wage: 3rd wage quart.")+ylim(-0.1, 0.05)
p4 = plot_es(m4, title = "Log Wage: 4th wage quart.")+ylim(-0.1, 0.05)
plot_grid(p1,p2, p3, p4, nrow = 2)

ggsave(paste0(path_save,"wages_rank_sal_cbo.png"), height = 5, width = 9)


######################################
# Admissoes e demissoes
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "admissao",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "demissao",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = 'Log New Hires')+ylim(-0.3, 0.1)
p2 = plot_es(m2, title = "Log Displacements") + ylim(-0.3, 0.1)
plot_grid(p1,p2, nrow = 1)

ggsave(paste0(path_save,"adm_dem.png"), height = 5, width = 9)


######################################
# Emprego Publico E Rural
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_publico",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "salario_publico",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = 'Public Employment')
p2 = plot_es(m2, title = "Log Public Wages")
plot_grid(p1,p2, nrow = 1)

ggsave(paste0(path_save,"emprego_salario_publico.png"), height = 5, width = 9)


######################################
# Frota de veículos
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "n_veics",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated")


plot_es(m1, title = 'Log Cars') %>% print()


ggsave(paste0(path_save,"frota_veics.png"), height = 5, width = 9)



######################################
# Emprego privado- Por Educação - BASE UNIVERSAL
######################################
m1 = regressao_cs(variavel_dependente = "emprego_lths",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")

m2 = regressao_cs(variavel_dependente = "emprego_hs_somecol",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")

m3 = regressao_cs(variavel_dependente = "emprego_col",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")

p1 = plot_es(m1, title = 'Less Than HS')+ylim(-0.15, 0.07)
p2 = plot_es(m2, title = "HS/ Some Coll.")+ylim(-0.15, 0.07)
p3 = plot_es(m3, title = "College")+ylim(-0.15, 0.07)
plot_grid(p1,p2,p3, nrow = 1)

ggsave(paste0(path_save,"emprego_privado_escolaridade_universal.png"), height = 5, width = 9)


######################################
# Log Salarios - Educ - base universal
######################################
m1 = regressao_cs(variavel_dependente = "salario_lths",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")
m2 = regressao_cs(variavel_dependente = "salario_hs_somecol",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")
m3 = regressao_cs(variavel_dependente = "salario_col",
                  dep_em_log= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")


p1 = plot_es(m1, title = "LTHS")+ylim(-0.08, 0.04)
p2 = plot_es(m2, title = 'HS/ Some Coll.')+ylim(-0.08, 0.04)
p3 = plot_es(m3, title = 'College')+ylim(-0.08, 0.04)
plot_grid(p1,p2, p3, nrow = 1)

ggsave(paste0(path_save,"wages_educ_universal.png"), height = 9, width = 9)


######################################
# Emprego privado- Por Rank de Salário CBO - base universal
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_baixo_cbo",
                  dep_em_log= 1, controles_use = controles, 
                  base_period = 'universal', 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_med_cbo",
                  dep_em_log= 1, controles_use = controles, 
                  base_period = 'universal', 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_alto_cbo",
                  dep_em_log= 1, controles_use = controles, 
                  base_period = 'universal', 
                  control_group = "notyettreated")

m4 = regressao_cs(variavel_dependente = "emprego_altissimo_cbo",
                  dep_em_log= 1, controles_use = controles, 
                  base_period = 'universal', 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = 'Log Emp: 1st wage quart.')+ylim(-0.15, 0.08)
p2 = plot_es(m2, title = "Log Emp: 2nd wage quart.")+ylim(-0.15, 0.08)
p3 = plot_es(m3, title = "Log Emp: 3rd wage quart.")+ylim(-0.15, 0.08)
p4 = plot_es(m4, title = "Log Emp: 4th wage quart.")+ylim(-0.15, 0.08)
plot_grid(p1,p2, p3, p4, nrow = 2)

ggsave(paste0(path_save,"emprego_privado_rank_sal_cbo_universal.png"), 
       height = 5, width = 9)


######################################
# Emprego privado- Por Salario - Base Universal
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_baixo_sal",
                  dep_em_log= 1, controles_use = controles,  
                  base_period = 'universal', 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_med_sal",
                  dep_em_log= 1, controles_use = controles,  
                  base_period = 'universal', 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_alto_sal",
                  dep_em_log= 1, controles_use = controles,  
                  base_period = 'universal', 
                  control_group = "notyettreated")

m4 = regressao_cs(variavel_dependente = "emprego_altissimo_sal",
                  dep_em_log= 1, controles_use = controles,  
                  base_period = 'universal', 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = 'Log Emp: (,1500]')+ylim(-0.2, 0.1)
p2 = plot_es(m2, title = "Log Emp: (1500,3000]")+ylim(-0.2, 0.1)
p3 = plot_es(m3, title = "Log Emp: [3000, 6000]")+ylim(-0.2, 0.1)
p4 = plot_es(m4, title = "Log Emp: (6000,)")+ylim(-0.2, 0.1)
plot_grid(p1,p2, p3, p4, nrow = 2)

ggsave(paste0(path_save,"emprego_privado_nivel_sal_universal.png"), height = 5, width = 9)


######################################
# Homicidios e Acidentes de transito
######################################
# m1 = regressao_cs(variavel_dependente = "homicidios_pc",
#                   controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m2 = regressao_cs(variavel_dependente = "mortes_acidente_carro_pc",
#                   controles_use = controles, dep_em_log =1,
#                   control_group = "notyettreated")
# p1 = plot_es(m1, title = 'Homicidios/100k Hab')
# p2 = plot_es(m2, title = "Mortes Acidente de Carros/ 100k Hab")
# pg = plot_grid(p1,p2, nrow = 1)
# title = ggdraw() + draw_label("Mortes - SIM-SUS", fontface='bold')
# plot_grid(title, pg, ncol=1, rel_heights=c(0.1, 1))
# 
# ggsave(paste0(path_save,"homicidios_acidentes.png"), height = 4.5, width = 8)


######################################
# Teste
######################################
set.seed(456)
df = df %>% 
  arrange(rgi, anosem) %>% data.table()
df[, dif_veic := n_veics - lag(n_veics), by = 'rgi']

m1 = regressao_cs(variavel_dependente = "tenure_privado",
                  dep_em_log= 1, 
                  # base_period = 'universal',
                  controles_use = controles, 
                  control_group = "notyettreated")
plot_es(m1, title = 'Teste') %>% print()


######################################
# verificar share de regioes para as quais eu tenho a
# data de entrada da maior cidade
######################################
#dados de microrregiao
micro = read_excel("../Data/regioes_geograficas.xlsx") %>% 
  select(2,3) %>% setDT()
colnames(micro) = c('id_municipio', 'rgi')
micro[, `:=`(id_municipio = as.integer(substr(id_municipio, 1, 6)),
             rgi = as.integer(rgi))]

#dados de populacao por municipio
pop_mun = read_parquet("../data/munic_data_10.parquet") %>% 
  select(munic, pop_m) %>% rename(id_municipio = munic) %>% data.table()
pop_mun[, id_municipio := as.integer(id_municipio)]

#entry dates
entry = read_excel("../data/initial_dates.xlsx", sheet = "032023") %>% 
  select(munic, problem, ever_treated) %>% data.table()
colnames(entry) = c('id_municipio', 'tenho_datas', 'tem_uber')
entry[, tenho_datas := fifelse(!is.na(tenho_datas), 1, 0)]
entry[, id_municipio := as.integer(substr(id_municipio, 1, 6))]

#adicionar pop e microrregiao no dataset de datas uber
entry = merge(entry, pop_mun, by = 'id_municipio', all.x = TRUE)

#adicionar info de regiao
entry = merge(entry, micro, by = 'id_municipio', all.x = TRUE)

#dropar unica cidade que n tem populacao no censo 2010 (balneario rincao, cidade pequena)
entry = entry[!is.na(pop_m)]


#definir maior cidade por regiao
entry[, maior_cidade := fifelse(pop_m == max(pop_m), 1, 0), by = 'rgi']

#definir se eu tenho datas de alguma cidade da regiao
entry[, tenho_alguma_data := max(tenho_datas), by = 'rgi']

#filtrar maiores cidades em regiões que tem uber e que eu tenha a data de pelo menos
# uma cidade
entry = entry[tenho_alguma_data == 1 & maior_cidade == 1]

count(entry, tenho_datas)



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
  
  # m2 = regressao_cs(variavel_dependente = "emprego_lths",
  #                   dep_em_log= 1, controles_use = controles, 
  #                   control_group = "notyettreated")
  # 
  # m3 = regressao_cs(variavel_dependente = "salario_hs",
  #                   dep_em_log= 1, controles_use = controles, 
  #                   control_group = "notyettreated")
  semestre = as.character(grupo)
  #pegar dados do primeiro modelo
  att1 = m1[[2]]$overall.att
  inf1 = (-1.96*m1[[2]]$overall.se)+att1
  sup1 = (1.96*m1[[2]]$overall.se)+att1
  
  # #pegar dados do segundo modelo
  # att2 = m2[[2]]$overall.att
  # inf2 = (-1.96*m2[[2]]$overall.se)+att2
  # sup2 = (1.96*m2[[2]]$overall.se)+att2
  # 
  # #pegar dados do terceiro modelo
  # att3 = m3[[2]]$overall.att
  # inf3 = (-1.96*m3[[2]]$overall.se)+att3
  # sup3 = (1.96*m3[[2]]$overall.se)+att3
  
  new_line = data.table(semestre, att1, inf1, sup1
                        # ,att2, inf2, sup2 
                        # ,att3, inf3, sup3
                        )
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
print(p1)

# p2 = ggplot(tabela_resultados, aes(x = semestre, y = att2))+
#   geom_point()+
#   geom_errorbar(aes(ymin = inf2, ymax = sup2), width = 0.1)+
#   labs(x = ' ', y = ' ', title = "Formal Emp. - LTHS")+
#   geom_hline(aes(yintercept =0))+
#   geom_hline(aes(yintercept = or_att_emp_lths), linetype = "dotted")+aux_theme
# 
# p3 = ggplot(tabela_resultados, aes(x = semestre, y = att3))+
#   geom_point()+
#   geom_errorbar(aes(ymin = inf3, ymax = sup3), width = 0.1)+
#   labs(x = ' ', y = ' ', title = "Wages - HS or More")+
#   geom_hline(aes(yintercept =0))+
#   geom_hline(aes(yintercept = or_att_wage_hs), linetype = "dotted")+aux_theme

# plot_grid(p1, p2, p3, nrow = 1)
ggsave(paste0(path_save,"leave_one_out.png"), height = 5, width = 9)


