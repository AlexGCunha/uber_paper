########################################
#This code will:
#- Estudar relação entre posse de carro e renda familiar
########################################
library(tidyverse)
library(data.table)
library(arrow)
library(readxl)
library(PNADcIBGE)

#funcoes auxiliares 
source("../uber/ub_funcoes_auxiliares.R")
#deflator
precos = read_excel("../data/deflator_inpc.xlsx", sheet = "anual_junho") %>% 
  data.table()
deflator = precos[ano == 2016]$deflator_24


#PNAD características gerais dos moradores 2016
pnad = get_pnadc(year = 2016, interview = 1, design = FALSE, labels = FALSE)
pnad = data.table(pnad)

old_names = c('V1032', 'V2001', 'VD4019', 'S010311', 'V1008', 'V1014')
new_names = c('peso', 'n_pes_dom', 'rendimento_total', 'possui_carro',
              'numero_dom', 'painel_dom')
setnames(pnad, old = old_names, new = new_names)

#transformar a base em domiciliar
pnad[, id_dom := paste0(UPA, numero_dom, painel_dom)]
pnad[, possui_carro := as.integer(possui_carro)]
pnad[, rendimento_total := rendimento_total * deflator]
pnad[, possui_carro := fcase(possui_carro == 1, 1, default = 0)]
pnad_dom = pnad[, .(n_pes_dom = mean(n_pes_dom),
                    rendimento_total = sum(rendimento_total, na.rm = TRUE),
                    possui_carro = mean(possui_carro),
                    peso_dom = mean(peso)),
                by = . (id_dom)]
sum(pnad_dom[possui_carro == 1]$peso_dom)


#funcao para calcular a probabilidade de ter carro dado uma renda x
prob_carro = function(x){
  x_min = x - 500
  x_max = x + 500
  
  df_aux = pnad_dom[ rendimento_total >= x_min & rendimento_total < x_max]
  prob_carro = sum(df_aux[possui_carro == 1]$peso_dom)/sum(df_aux$peso_dom)
  
  return(prob_carro)
  
}

#funcao para calcular a probabilidade acumulada de ter carro dada uma renda x
prob_cum = function(x){
  #filtrar pessoas que ganham até x
  df_aux = pnad_dom[rendimento_total <= x]
  
  prob_cum_carro = sum(df_aux[possui_carro == 1]$peso_dom)/sum(df_aux$peso_dom)
  
  return(prob_cum_carro)
}

#dataset com salarios e probabilidade de ter carro
salarios = seq(0, 22000, 1000)
probs = data.table(faixa_salario = salarios, 
                   prob_carro_faixa = sapply(salarios, prob_carro),
                   prob_carro_cum = sapply(salarios, prob_cum)) 



#Gráfico probabilidade por faixa
p1 = ggplot(probs, aes(x = faixa_salario, y = prob_carro_faixa))+geom_point()+
  tema_padrao+
  labs(title = '(A)', x = 'Household Earnings Range', 
       y ='Probability of Owning a Car')+xlim(0, 23000)+ylim(0, 1)


