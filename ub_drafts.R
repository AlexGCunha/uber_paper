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
library(readxl)


#Abrir dataset
df = read_parquet('../data/ub_final_dataset.parquet')
source("../uber/ub_funcoes_auxiliares.R")

#definir controles
# controles = as.formula(" ~  lincome_r  + unem_rate_r   +lpop_r +lemprego_14 ")
controles = as.formula(" ~  lincome_r  + unem_rate_r   +lpea_r +lemprego_14 ")
sem_controles = as.formula("~ 1")

#definir pasta
data = "202509"
path_save = paste0("../Output/",data,"/")

######################################
#Emprego privado
######################################
m2 = regressao_cs(variavel_dependente = "emprego_privado",
                  # dep_em_log = 1,
                  dep_relativo_2014= 1,
                  controles_use = controles,
                  # base_period = 'universal',
                  control_group = "notyettreated")

p2 = plot_es(m2, title = " ", lim_y = c(-0.1,0.05))
p2 %>% print()
or_att_emp = m2[[2]]$overall.att

ggsave(paste0(path_save,"emprego_noeduc.png"), height = 5, width = 9)

######################################
# Emprego privado- Por Educação
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_lths",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_hs_somecol",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_col",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = 'Less Than HS',lim_y = c(-0.15, 0.07))
p2 = plot_es(m2, title = "HS/ Some Coll.", lim_y = c(-0.15, 0.07))
p3 = plot_es(m3, title = "College or More", lim_y = c(-0.15, 0.07))
plot_grid(p1,p2, p3, nrow = 1)

#save original overall att for lths to use later
or_att_emp_lths = m1[[2]]$overall.att

ggsave(paste0(path_save,"emprego_privado_escolaridade.png"), height = 5, width = 9)


######################################
# Emprego privado- Por Salario
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_baixo_sal",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_med_sal",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_alto_sal",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m4 = regressao_cs(variavel_dependente = "emprego_altissimo_sal",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")


p1 = plot_es(m1, title = '(,2000]', lim_y = c(-0.3, 0.1))
p2 = plot_es(m2, title = "(2000,6000]", lim_y = c(-0.3, 0.1))
p3 = plot_es(m3, title = "[6000, 10000]", lim_y = c(-0.3, 0.1))
p4 = plot_es(m4, title = "(10000,)", lim_y = c(-0.3, 0.1))
plot_grid(p1,p2, p3, p4, nrow = 2)

#save original att for mid wages
or_att_mid_wage = m2[[2]]$overall.att

ggsave(paste0(path_save,"emprego_privado_nivel_sal.png"), height = 5, width = 9)


######################################
# Emprego LTHS- Por Salario
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_lths_baixo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_lths_med",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_lths_alto",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m4 = regressao_cs(variavel_dependente = "emprego_lths_altissimo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")


p1 = plot_es(m1, title = '(,2000]', lim_y = c(-0.7, 0.25))
p2 = plot_es(m2, title = "(2000,6000]", lim_y = c(-0.7, 0.25))
p3 = plot_es(m3, title = "[6000, 10000]", lim_y = c(-0.7, 0.25))
p4 = plot_es(m4, title = "(10000,)", lim_y = c(-0.7, 0.25))
plot_grid(p1,p2, p3, p4, nrow = 2)

ggsave(paste0(path_save,"emprego_lths_nivel_sal.png"), height = 5, width = 9)


######################################
# Emprego HS Some Col- Por Salario
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_hs_baixo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_hs_med",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_hs_alto",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m4 = regressao_cs(variavel_dependente = "emprego_hs_altissimo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")


p1 = plot_es(m1, title = '(,2000]', lim_y = c(-0.25, 0.15))
p2 = plot_es(m2, title = "(2000,6000]", lim_y = c(-0.25, 0.15))
p3 = plot_es(m3, title = "[6000, 10000]", lim_y = c(-0.25, 0.15))
p4 = plot_es(m4, title = "(10000,)", lim_y = c(-0.25, 0.15))
plot_grid(p1,p2, p3, p4, nrow = 2)

ggsave(paste0(path_save,"emprego_hs_nivel_sal.png"), height = 5, width = 9)


######################################
# Emprego College- Por Salario
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_col_baixo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_col_med",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_col_alto",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m4 = regressao_cs(variavel_dependente = "emprego_col_altissimo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")


p1 = plot_es(m1, title = '(,2000]', lim_y = c(-0.3, 0.2))
p2 = plot_es(m2, title = "(2000,6000]", lim_y = c(-0.3, 0.2))
p3 = plot_es(m3, title = "[6000, 10000]", lim_y = c(-0.3, 0.2))
p4 = plot_es(m4, title = "(10000,)", lim_y = c(-0.3, 0.2))
plot_grid(p1,p2, p3, p4, nrow = 2)

ggsave(paste0(path_save,"emprego_col_nivel_sal.png"), height = 5, width = 9)

######################################
# Emprego privado- Por Sexo
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_homens",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_mulheres",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = 'Men', lim_y = c(-0.15, 0.07))
p2 = plot_es(m2, title = "Women", lim_y = c(-0.15, 0.07))
plot_grid(p1,p2, nrow = 1)

#save original att for men
or_att_men = m1[[2]]$overall.att

ggsave(paste0(path_save,"emprego_privado_sexo.png"), height = 5, width = 9)

######################################
# Emprego privado- Por Idade
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_baixo_idade",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_med_idade",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_alto_idade",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = '(,30]', lim_y = c(-0.10, 0.05))
p2 = plot_es(m2, title = "(30,45]", lim_y = c(-0.1, 0.05))
p3 = plot_es(m3, title = "(45,)", lim_y = c(-0.10, 0.05))
plot_grid(p1,p2, p3, nrow = 1)

ggsave(paste0(path_save,"emprego_privado_idade.png"), height = 5, width = 9)


######################################
# Emprego privado- Por Rank de Salário CBO
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_baixo_cbo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_med_cbo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_alto_cbo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m4 = regressao_cs(variavel_dependente = "emprego_altissimo_cbo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = '1st wage quart.', lim_y = c(-0.15, 0.05))
p2 = plot_es(m2, title = "2nd wage quart.", lim_y = c(-0.15, 0.05))
p3 = plot_es(m3, title = "3rd wage quart.", lim_y = c(-0.15, 0.05))
p4 = plot_es(m4, title = "4th wage quart.", lim_y = c(-0.15, 0.05))
plot_grid(p1,p2, p3, p4, nrow = 2)

ggsave(paste0(path_save,"emprego_privado_rank_sal_cbo.png"), height = 5, width = 9)


######################################
# Emprego privado- Por investimento em RD (OCDE)
######################################
set.seed(456)
m1 = regressao_cs(variavel_dependente = "emprego_hintensity",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_lintensity",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")


p2 = plot_es(m1, title = 'High Research Sectors', lim_y = c(-0.20, 0.1))
p1 = plot_es(m2, title = "Low Research Sectors.", lim_y = c(-0.20, 0.1))
plot_grid(p1,p2)

ggsave(paste0(path_save,"emprego_inv_pesquisa.png"), height = 5, width = 9)


######################################
# Emprego privado- Por quantil de share com college
######################################
set.seed(123)
m1 = regressao_cs(variavel_dependente = "emprego_lcol",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_mcol",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_hcol",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = 'Low College Sect.', lim_y = c(-0.15, 0.1))
p2 = plot_es(m2, title = "Mid College Sect.", lim_y = c(-0.15, 0.1))
p3 = plot_es(m3, title = "High College Sect.", lim_y = c(-0.15, 0.1))
plot_grid(p1,p2, p3, nrow = 1)

ggsave(paste0(path_save,"emprego_share_col.png"), height = 5, width = 9)

######################################
# Emprego temporário e Meio Período
######################################
set.seed(123)
m1 = regressao_cs(variavel_dependente = "emprego_meio_periodo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = " ", lim_y = c(-0.2, 0.3))
print(p1)

ggsave(paste0(path_save,"empregos_alternativos.png"), height = 5, width = 9)


######################################
# Log Salarios
######################################
set.seed(123)
m1 = regressao_cs(variavel_dependente = "salario_privado",
                  dep_relativo_2014 = 1,
                  controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = ' ', lim_y = c(-0.06, 0.04))
p1 %>% print()
ggsave(paste0(path_save,"wages.png"), height = 4.5, width = 8)

######################################
# Log Salarios - Educ
######################################
set.seed(123)
m1 = regressao_cs(variavel_dependente = "salario_lths",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")
m2 = regressao_cs(variavel_dependente = "salario_hs_somecol",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "salario_col",
                  dep_relativo_2014= 1, controles_use = controles, 
                  # base_period = 'universal',
                  control_group = "notyettreated")



p1 = plot_es(m1, title = "Less Than High School", lim_y = c(-0.1, 0.05))
p2 = plot_es(m2, title = 'HS/ Some College.', lim_y = c(-0.1, 0.05))
p3 = plot_es(m3, title = 'College', lim_y = c(-0.1, 0.05))
plot_grid(p1,p2, p3, nrow = 1)

ggsave(paste0(path_save,"wages_educ.png"), height = 5, width = 9)

######################################
# Log Salario- Por Rank de Salário CBO
######################################
set.seed(123)
m1 = regressao_cs(variavel_dependente = "salario_baixo_cbo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "salario_med_cbo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "salario_alto_cbo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m4 = regressao_cs(variavel_dependente = "salario_altissimo_cbo",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = '1st wage quart.', lim_y = c(-0.1, 0.05))
p2 = plot_es(m2, title = "2nd wage quart.", lim_y = c(-0.1, 0.05))
p3 = plot_es(m3, title = "3rd wage quart.", lim_y = c(-0.1, 0.05))
p4 = plot_es(m4, title = "4th wage quart.", lim_y = c(-0.1, 0.05))
plot_grid(p1,p2, p3, p4, nrow = 2)

ggsave(paste0(path_save,"wages_rank_sal_cbo.png"), height = 5, width = 9)


######################################
# Admissoes e demissoes
######################################
set.seed(123)
m1 = regressao_cs(variavel_dependente = "admitido",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "demitido",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = 'New Hires', lim_y = c(-0.3, 0.1))
p2 = plot_es(m2, title = "Displacements", lim_y = c(-0.3, 0.1)) 
plot_grid(p1,p2, nrow = 1)

ggsave(paste0(path_save,"adm_dem.png"), height = 5, width = 9)

######################################
# Admissoes- Por Salario
# ######################################
# set.seed(123)
# m1 = regressao_cs(variavel_dependente = "admitido_baixo_sal",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m2 = regressao_cs(variavel_dependente = "admitido_med_sal",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m3 = regressao_cs(variavel_dependente = "admitido_alto_sal",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m4 = regressao_cs(variavel_dependente = "admitido_altissimo_sal",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# p1 = plot_es(m1, title = 'Log Hires: (,1500]'), lim_y = c(-0.45, 0.25)
# p2 = plot_es(m2, title = "Log Hires: (1500,3000]"), lim_y = c(-0.45, 0.25)
# p3 = plot_es(m3, title = "Log Hires: [3000, 6000]"), lim_y = c(-0.45, 0.25)
# p4 = plot_es(m4, title = "Log Hires: (6000,)"), lim_y = c(-0.45, 0.25)
# plot_grid(p1,p2, p3, p4, nrow = 2)
# 
# ggsave(paste0(path_save,"admissoes_nivel_sal.png"), height = 5, width = 9)

######################################
# Demissoes- Por Salario
# ######################################
# set.seed(123)
# m1 = regressao_cs(variavel_dependente = "demitido_baixo_sal",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m2 = regressao_cs(variavel_dependente = "demitido_med_sal",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m3 = regressao_cs(variavel_dependente = "demitido_alto_sal",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m4 = regressao_cs(variavel_dependente = "demitido_altissimo_sal",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# p1 = plot_es(m1, title = 'Log Layoffs: (,1500]'), lim_y = c(-0.5, 0.3)
# p2 = plot_es(m2, title = "Log Layoffs: (1500,3000]"), lim_y = c(-0.5, 0.3)
# p3 = plot_es(m3, title = "Log Layoffs: [3000, 6000]"), lim_y = c(-0.5, 0.3)
# p4 = plot_es(m4, title = "Log Layoffs: (6000,)"), lim_y = c(-0.5, 0.3)
# plot_grid(p1,p2, p3, p4, nrow = 2)
# 
# ggsave(paste0(path_save,"demissoes_nivel_sal.png"), height = 5, width = 9)


######################################
# Admissoes e demissoes - CAGED
######################################
set.seed(123)
m1 = regressao_cs(variavel_dependente = "admissao_total",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "demissao_total",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = 'New Hires', lim_y = c(-0.4, 0.2))
p2 = plot_es(m2, title = "Displacements", lim_y = c(-0.4, 0.2)) 
plot_grid(p1,p2, nrow = 1)

ggsave(paste0(path_save,"adm_dem_caged.png"), height = 5, width = 9)

# ######################################
# # Admissoes- Por Salario - CAGED
# ######################################
# set.seed(123)
# m1 = regressao_cs(variavel_dependente = "admissao_baixo",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m2 = regressao_cs(variavel_dependente = "admissao_med",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m3 = regressao_cs(variavel_dependente = "admissao_alto",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m4 = regressao_cs(variavel_dependente = "admissao_altissimo",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# p1 = plot_es(m1, title = '(,2000]', lim_y = c(-0.45, 0.25))
# p2 = plot_es(m2, title = "(2000,6000]", lim_y = c(-0.45, 0.25))
# p3 = plot_es(m3, title = "[6000, 10000]", lim_y = c(-1, 0.25))
# p4 = plot_es(m4, title = "(10000,)", lim_y = c(-1, 0.25))
# plot_grid(p1,p2, p3, p4, nrow = 2)
# 
# ggsave(paste0(path_save,"admissoes_nivel_sal_caged.png"), height = 5, width = 9)
# 
# ######################################
# # Demissoes- Por Salario - CAGED
# ######################################
# set.seed(123)
# m1 = regressao_cs(variavel_dependente = "demissao_baixo",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m2 = regressao_cs(variavel_dependente = "demissao_med",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m3 = regressao_cs(variavel_dependente = "demissao_alto",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# m4 = regressao_cs(variavel_dependente = "demissao_altissimo",
#                   dep_relativo_2014= 1, controles_use = controles, 
#                   control_group = "notyettreated")
# 
# p1 = plot_es(m1, title = 'Log Layoffs: (,2000]', lim_y = c(-0.5, 0.3))#
# p2 = plot_es(m2, title = "Log Layoffs: (2000,6000]", lim_y = c(-0.5, 0.3))#
# p3 = plot_es(m3, title = "Log Layoffs: [6000, 10000]", lim_y = c(-0.5, 0.3))#
# p4 = plot_es(m4, title = "Log Layoffs: (10000,)", lim_y = c(-0.5, 0.3))#
# plot_grid(p1,p2, p3, p4, nrow = 2)
# 
# ggsave(paste0(path_save,"demissoes_nivel_sal_caged.png"), height = 5, width = 9)



######################################
# Emprego Publico E Rural
######################################
set.seed(123)
m1 = regressao_cs(variavel_dependente = "emprego_publico",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "salario_publico",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")


p1 = plot_es(m1, title = 'Employment', lim_y = c(-0.08, 0.1))
p2 = plot_es(m2, title = "Wages", lim_y = c(-0.08, 0.1))
plot_grid(p1,p2, nrow = 1)

ggsave(paste0(path_save,"emprego_salario_publico.png"), height = 5, width = 9)


######################################
# Frota de veículos
######################################
set.seed(123)
m1 = regressao_cs(variavel_dependente = "n_veics",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated")


plot_es(m1, title = 'Log Cars', lim_y = c(-0.025, 0.025)) %>% print()


ggsave(paste0(path_save,"frota_veics.png"), height = 5, width = 9)



######################################
# Emprego privado- Por Educação - BASE UNIVERSAL
######################################
m1 = regressao_cs(variavel_dependente = "emprego_lths",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")

m2 = regressao_cs(variavel_dependente = "emprego_hs_somecol",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")

m3 = regressao_cs(variavel_dependente = "emprego_col",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")

p1 = plot_es(m1, title = 'Less Than HS', lim_y = c(-0.15, 0.07))
p2 = plot_es(m2, title = "HS/ Some Coll.", lim_y = c(-0.15, 0.07))
p3 = plot_es(m3, title = "College", lim_y = c(-0.15, 0.07))
plot_grid(p1,p2,p3, nrow = 1)

ggsave(paste0(path_save,"emprego_privado_escolaridade_universal.png"), height = 5, width = 9)


######################################
# Log Salarios - Educ - base universal
######################################
m1 = regressao_cs(variavel_dependente = "salario_lths",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")
m2 = regressao_cs(variavel_dependente = "salario_hs_somecol",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")
m3 = regressao_cs(variavel_dependente = "salario_col",
                  dep_relativo_2014= 1, controles_use = controles, 
                  control_group = "notyettreated",
                  base_period = "universal")


p1 = plot_es(m1, title = "Less Than HS", lim_y = c(-0.1, 0.05))
p2 = plot_es(m2, title = 'HS/ Some Coll.', lim_y = c(-0.1, 0.05))
p3 = plot_es(m3, title = 'College', lim_y = c(-0.1, 0.05))
plot_grid(p1,p2, p3, nrow = 1)

ggsave(paste0(path_save,"wages_educ_universal.png"), height = 9, width = 9)





######################################
# Emprego privado- Por Salario - Base Universal
######################################
set.seed(123)
m1 = regressao_cs(variavel_dependente = "emprego_baixo_sal",
                  dep_relativo_2014= 1, controles_use = controles,  
                  base_period = 'universal', 
                  control_group = "notyettreated")

m2 = regressao_cs(variavel_dependente = "emprego_med_sal",
                  dep_relativo_2014= 1, controles_use = controles,  
                  base_period = 'universal', 
                  control_group = "notyettreated")

m3 = regressao_cs(variavel_dependente = "emprego_alto_sal",
                  dep_relativo_2014= 1, controles_use = controles,  
                  base_period = 'universal', 
                  control_group = "notyettreated")

m4 = regressao_cs(variavel_dependente = "emprego_altissimo_sal",
                  dep_relativo_2014= 1, controles_use = controles,  
                  base_period = 'universal', 
                  control_group = "notyettreated")

p1 = plot_es(m1, title = '(,2000]', lim_y = c(-0.3, 0.1))
p2 = plot_es(m2, title = "(2000,6000]", lim_y = c(-0.3, 0.1))
p3 = plot_es(m3, title = "[6000, 10000]", lim_y = c(-0.3, 0.1))
p4 = plot_es(m4, title = "(10000,)", lim_y = c(-0.3, 0.1))
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
#                   controles_use = controles, dep_relativo_2014 =1,
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
set.seed(123)
# controles = as.formula(" ~  lincome_r  + unem_rate_r   +lpop_r +lemprego_14 ")

df[, emprego_teste := emprego_6 - emprego_10]

m1 = regressao_cs(variavel_dependente = "emprego_4",
                  dep_relativo_2014= 1,
                  # dep_relativo_2014 = 1,
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
source("../uber/ub_funcoes_auxiliares.R")
grupos = df %>% 
  filter(!is.na(semestre_entrada)) %>% 
  arrange(semestre_entrada) %>% 
  select(semestre_entrada) %>% unique() %>% pull()

df_back = copy(df)
tabela_resultados = data.table()

for(grupo in grupos){
  df = df_back[!semestre_entrada %in% grupo]
  m1 = regressao_cs(variavel_dependente = "emprego_privado",
                    dep_relativo_2014= 1, controles_use = controles, 
                    control_group = "notyettreated")
  
  m2 = regressao_cs(variavel_dependente = "emprego_lths",
                    dep_relativo_2014= 1, controles_use = controles,
                    control_group = "notyettreated")
  
  m3 = regressao_cs(variavel_dependente = "emprego_med_sal",
                    dep_relativo_2014= 1, controles_use = controles,
                    control_group = "notyettreated")
  
  m4 = regressao_cs(variavel_dependente = "emprego_homens",
                    dep_relativo_2014= 1, controles_use = controles,
                    control_group = "notyettreated")
  semestre = as.character(grupo)
  #pegar dados do primeiro modelo
  att1 = m1[[2]]$overall.att
  inf1 = (-1.96*m1[[2]]$overall.se)+att1
  sup1 = (1.96*m1[[2]]$overall.se)+att1
  
  # #pegar dados do segundo modelo
  att2 = m2[[2]]$overall.att
  inf2 = (-1.96*m2[[2]]$overall.se)+att2
  sup2 = (1.96*m2[[2]]$overall.se)+att2
  
  #pegar dados do terceiro modelo
  att3 = m3[[2]]$overall.att
  inf3 = (-1.96*m3[[2]]$overall.se)+att3
  sup3 = (1.96*m3[[2]]$overall.se)+att3
  
  #pegar dados do terceiro modelo
  att4 = m4[[2]]$overall.att
  inf4 = (-1.96*m4[[2]]$overall.se)+att3
  sup4 = (1.96*m4[[2]]$overall.se)+att3
  
  new_line = data.table(semestre, att1, inf1, sup1
                        ,att2, inf2, sup2
                        ,att3, inf3, sup3
                        ,att4, inf4, sup4
  )
  tabela_resultados = rbind(tabela_resultados, new_line)
  df = copy(df_back)
}

source("../uber/ub_funcoes_auxiliares.R")
aux_theme = theme_classic() +tema_padrao+
  theme(plot.title = element_text(color="black"),
        axis.title = element_text(color="black"),
        strip.background = element_rect(fill = 'white', color = 'white'),
        strip.text = element_text(color = 'darkgray', face = 'bold', size = 9, hjust = 0),
        legend.position = 'bottom',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


p1 = ggplot(tabela_resultados, aes(x = semestre, y = att1))+
  geom_point()+
  geom_errorbar(aes(ymin = inf1, ymax = sup1), width = 0.1)+
  labs(x = ' ', y = ' ', title = "Everyone")+
  geom_hline(aes(yintercept =0))+
  geom_hline(aes(yintercept = or_att_emp), linetype = "dotted")+aux_theme


p2 = ggplot(tabela_resultados, aes(x = semestre, y = att2))+
  geom_point()+
  geom_errorbar(aes(ymin = inf2, ymax = sup2), width = 0.1)+
  labs(x = ' ', y = ' ', title = "Less Than High School")+
  geom_hline(aes(yintercept =0))+
  geom_hline(aes(yintercept = or_att_emp_lths), linetype = "dotted")+aux_theme

p3 = ggplot(tabela_resultados, aes(x = semestre, y = att3))+
  geom_point()+
  geom_errorbar(aes(ymin = inf3, ymax = sup3), width = 0.1)+
  labs(x = ' ', y = ' ', title = "Earnigns :(2000,6000]")+
  geom_hline(aes(yintercept =0))+
  geom_hline(aes(yintercept = or_att_mid_wage), linetype = "dotted")+aux_theme

p4 = ggplot(tabela_resultados, aes(x = semestre, y = att4))+
  geom_point()+
  geom_errorbar(aes(ymin = inf4, ymax = sup4), width = 0.1)+
  labs(x = ' ', y = ' ', title = "Men")+
  geom_hline(aes(yintercept =0))+
  geom_hline(aes(yintercept = or_att_men), linetype = "dotted")+aux_theme

plot_grid(p1, p2, p3, p4, nrow = 2)
ggsave(paste0(path_save,"leave_one_out.png"), height = 5, width = 9)


######################################
#Calcular probabilidade de demissao na minha amostra
######################################
demissoes_142 = sum(df[anosem == 20142, .(demitido)])
emprego_141 = sum(df[anosem == 20141, .(emprego_privado)])
prob_demissao_6m = demissoes_142/emprego_141
prob_demissao_1m = 1 - (1-prob_demissao_6m) ^(1/6)


#calcular share empregado formalmente
emprego_142 = sum(df[anosem == 20142, .(emprego_privado)])
pea_10 = sum(df[anosem == 20142, .(pea_r)])
pop_10 = sum(df[anosem == 20142, .(pop_r)])
pop_14 = sum(df[anosem == 20142, .(pop_14)])
pea_14 = (pop_14/pop_10) * pea_10
share_empregado = emprego_142/pea_14

teste = df[anosem == 20162, 
           .(rgi, anosem, emprego_privado, emprego_lths,
             emprego_hs_somecol, emprego_col,
             emprego_lths_baixo, emprego_lths_med, emprego_lths_alto,
             emprego_lths_altissimo)]
