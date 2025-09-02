library(tidyverse)
library(data.table)
library(arrow)
library(readxl)
#############
#arquivo temporario para converter dados
#deflator trimestral
df = read_excel('C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Job Displacement/data/series_nacionais.xlsx', sheet = 'trimestral')
write_csv(df, '../data/series_nacionais_tri.csv')
rm(df)

#aux datasets
precos = read_excel("../data/deflator_inpc.xlsx", sheet = "anual_junho")
cbo = read_parquet("../data/cbo_3digs.parquet")
rd_index = read_excel("../data/oecd_index.xlsx", sheet = 'cnae_index')
share_col = read_parquet("../data/share_college_cnae.parquet")

write_csv(precos, "../data/deflator_inpc.csv")
write_csv(cbo, "../data/cbo_3digs.csv")
write_csv(rd_index,  "../data/oecd_index.csv")
write_csv(share_col, "../data/share_college_cnae.csv")
rm(precos, cbo, rd_index, share_col)

#dados microrregiao
micro = read_excel('../data/regioes_geograficas.xlsx')
write_csv(micro, '../data/regioes_geograficas.csv')
rm(micro)

#datas uber
datas = read_excel("../data/initial_dates.xlsx", sheet = "032023")
write_csv(datas, "../data/initial_dates.csv")
rm(datas)

#populacao
populacao = read_excel("../data/time_series_pop.xlsx")
write_csv(populacao, "../data/time_series_pop.csv")
rm(populacao)

#censo
censo = read_parquet("../data/rgi_data_10.parquet")
write_csv(censo, "../data/rgi_data_10.csv")
rm(censo)

#munic data censo
pop_mun = read_parquet("../data/munic_data_10.parquet") 
write_csv(pop_mun, "../data/munic_data_10.csv") 
rm(pop_mun)




########################
#Converter de volta - AINDA POR FAZER
#######################
#deflator trimestral
df = read_csv('../data/series_nacionais_tri.csv')
write_parquet(df, '../data/series_nacionais_tri.parquet')
rm(df)

#aux datasets
precos = read_csv("../data/deflator_inpc.csv")
cbo = read_csv("../data/cbo_3digs.csv")
rd_index = read_csv( "../data/oecd_index.csv")
share_col = read_csv("../data/share_college_cnae.csv")

write_parquet(precos, "../data/deflator_inpc.parquet")
write_parquet(cbo, "../data/cbo_3digs.parquet")
write_parquet(rd_index,  "../data/oecd_index.parquet")
write_parquet(share_col, "../data/share_college_cnae.parquet")
rm(precos, cbo, rd_index, share_col)

#dados microrregiao
micro = read_csv('../data/regioes_geograficas.csv')
write_parquet(micro, '../data/regioes_geograficas.parquet')
rm(micro)

#datas uber
datas = read_csv("../data/initial_dates.csv")
write_parquet(datas, "../data/initial_dates.parquet")
rm(datas)

#populacao
populacao = read_csv("../data/time_series_pop.csv")
write_parquet(populacao, "../data/time_series_pop.parquet")
rm(populacao)

#censo
censo = read_csv("../data/rgi_data_10.csv")
write_parquet(censo, "../data/rgi_data_10.parquet")
rm(censo)

#munic data censo
pop_mun = read_csv("../data/munic_data_10.csv") 
write_parquet(pop_mun, "../data/munic_data_10.parquet") 
rm(pop_mun)