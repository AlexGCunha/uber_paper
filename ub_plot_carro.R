########################################
#DESCONTINUADO!!! SUBSTITUÍDO PELO CÓDIGO DA PNAD
#This code will:
#- Estudar relação entre posse de carro e renda familiar
########################################

library(tidyverse)
library(arrow)
library(readxl)
library(fixest)
library(data.table)

#deflator
precos = read_excel("../data/deflator_inpc.xlsx", sheet = "anual_junho") %>% 
  data.table()
deflator = precos[ano == 2010]$deflator_24

df = read_parquet("../data/census_dom_2010.parquet") %>% 
  data.table()

#Definir se possui carro
df[, possui_carro := fcase(automovel_particular == 1, 1, default = 0)]
df[, automovel_particular := NULL]   


# #deflacionar renda
df[, `:=`(rendimento_dom = rendimento_dom * deflator,
          rendimento_dom_pc = rendimento_dom_pc * deflator)]


#funcao para calcular a prob acumulada de ter carro por cada nível de renda
prob_cum = function(x){
  pop_x = sum(df[rendimento_dom <= x]$weight)
  possui_carro_x = sum(df[rendimento_dom <=x & possui_carro == 1]$weight)
  return(possui_carro_x/pop_x)
}

#funcao para calcular a prob de ter carro, por intervalo de renda, centrado em x
prob_intervalo = function(x){
  x_min = x - 250
  x_max = x +250
  
  pop_x = sum(df[rendimento_dom <= x_max & rendimento_dom > x_min]$weight)
  possui_carro_x = sum(df[rendimento_dom <= x_max & rendimento_dom > x_min & possui_carro == 1] $weight)
  return(possui_carro_x/pop_x)
}


#Cridar dataframe com distribuicao de prob de ter carro condicionado à renda
rendas = seq(0,30000,500)
dist_carro = data.table(
  renda = rendas,
  prob_carro = sapply(rendas, prob_cum),
  prob_carro_intervalo = sapply(rendas, prob_intervalo)
)

dist_carro[, dif := round(prob_carro - lag(prob_carro),4)]

#pontos adicionais
# pontos_adicionais = seq(2000, (2000+9*500), 500)
# probs_adicionais = sapply(pontos_adicionais, prob_intervalo)
# probs_cum_adicionais = sapply(pontos_adicionais, prob_cum)
# pontos = data.table(renda = pontos_adicionais,
#                     prob_carro = probs_cum_adicionais,
#                     prob_carro_intervalo = probs_adicionais)


ggplot()+
  geom_point(data = dist_carro, aes(x = renda, y = prob_carro_intervalo))
