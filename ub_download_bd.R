#------------------------------------------------------------------------------------------------
#This code will download Data from Base dos Dados
#------------------------------------------------------------------------------------------------

library(basedosdados)
library(tidyverse)
library(arrow)
library(readxl)
library(beepr)
rm(list=ls())

#Define WD
data_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/Uber effects/Data"
rais_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/RAIS"
# setwd(data_path)
#Define project
set_billing_id("ra-ubi-informality")

funcao_para_tentar = function(query, arquivo, ano){
  write_parquet(read_sql(query),arquivo)
  print(paste0(ano))
}
#############################
#RAIS
#############################
for (ano in 2012:2012){
  query = paste0("SELECT ano,
        id_municipio,
        mes_admissao,
        mes_desligamento,
        motivo_desligamento,
        tempo_emprego,
        quantidade_horas_contratadas,
        tipo_vinculo,
        tipo_admissao,
        natureza_juridica,
        cnae_2,
        idade,
        grau_instrucao_1985_2005,
        grau_instrucao_apos_2005,
        sexo,
        cbo_2002,
        raca_cor,
        valor_remuneracao_media,
        valor_remuneracao_dezembro
        FROM basedosdados.br_me_rais.microdados_vinculos
        WHERE ano = ", ano)
  arquivo = paste0(rais_path, "/rais_",ano,".parquet")
  
  #tentar rodar varias vezes
  r = NULL
  attempt = 1
  while(is.null(r) && attempt <= 3) {
    attempt <- attempt + 1
    try(
      funcao_para_tentar(query, arquivo, ano)
    )
  } 
  
  gc()
  
}

############################################################
#2010 Census
############################################################
#Individuals
query <- "SELECT id_municipio as munic,
  peso_amostral as weight,
  situacao_setor as urban_rural, 
  id_microrregiao as microreg,
  controle as control,  
  v0601 as gender,
  v0606 as ethnicity,
  v0618 as born_munic,
  v0624 as munic_time, 
  v0628 as in_school,
  v0633 as highest_degree,
  v0634 as conc_highest_degree,
  v0637 as lives_wpartner, 
  v0639 as civ_status, 
  v0641 as worked_ref_week, 
  v0642 as npaid1,
  v0643 as npaid2, 
  v0644 as npaid3, 
  v0648 as position, 
  v0650 as contrib_soc_sec,
  v0653 as wk_hours,
  v0654 as tried_job,
  v0655 as would_accept_job,
  v6036 as age,
  v6254 as munic_prev,
  v6264 as munic_2005,
  v6400 as instruct_level,
  v6461 as occupation,
  v6462 as occupation_cbo, 
  v6471 as sector,
  v6472 as sector_2000,
  v6511 as value_inc_main_job, 
  v6513 as inc_main_job, 
  v6514 as income_min_wage, 
  v6521 as value_other_income, 
  v6525 as total_income
  FROM basedosdados.br_ibge_censo_demografico.microdados_pessoa_2010"


write_parquet(read_sql(query),"../data/census_pes_2010.parquet")





#############################
#DataSUS
#############################
#Dictionary
query <- "SELECT *
  FROM basedosdados.br_ms_sim.dicionario
"


write_excel_csv(read_sql(query),"SIM_dict.csv")
gc()
beep()

#Mortality data - SIMSUS
query <- "SELECT ano,
data_obito,
sequencial_obito,
causa_basica,
circunstancia_obito,
idade,
sexo,
raca_cor,
id_municipio_ocorrencia,
FROM basedosdados.br_ms_sim.microdados
WHERE ano IN (2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)

"

write_parquet(read_sql(query),"SIM.parquet")
gc()
beep()

#Produção Ambulatorial
query <- "SELECT ano,
mes,
id_municipio,
cid_principal_categoria,
cid_secundario_categoria,
cid_causas_associadas_categoria,
sum(valor_produzido_procedimento) as valor_producao,
sum(valor_aprovado_procedimento) as valor_aprovado,
sum(valor_unitario_procedimento_vpa) as valor_vpa,
sum(valor_unitario_procedimento_sigtap) as valor_sigtap,
sum(valor_complemento_federal) as valor_comp_federal,
sum(valor_complemento_local) as valor_comp_local
FROM basedosdados.br_ms_sia.producao_ambulatorial
WHERE ano IN (2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
GROUP BY ano, mes, id_municipio, cid_principal_categoria, cid_secundario_categoria, cid_causas_associadas_categoria
"

arquivo = paste0(data_path, "/sus_ambulatorio.parquet")
write_parquet(read_sql(query),arquivo)


#############################
#CAGED
#############################

query <- "SELECT ano, mes, id_municipio, grau_instrucao, saldo_movimentacao, count(saldo_movimentacao) as saldo, avg(salario_mensal) as wages
FROM `basedosdados.br_me_caged.microdados_antigos` 
WHERE ano IN (2010,2011,2012,2013,2014,2015,2016,2017,2018,2019) AND indicador_aprendiz = '0'

GROUP BY ano, mes, id_municipio, grau_instrucao,saldo_movimentacao


"


write_parquet(read_sql(query),"CAGED_mod.parquet")
gc()
beep()


#############################
#Frota de Veículos
#############################

query <- "SELECT sigla_uf,id_municipio, ano, mes,automovel, total
FROM `basedosdados.br_denatran_frota.municipio_tipo` 
WHERE ano IN (2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
"
write_parquet(read_sql(query),"FROTA.parquet")
gc()
beep()



#############################
#ESTBAN
#############################
for (ano in 2011:2019){
  ano = as.character(ano)
  print(Sys.time())
  
  query_a <- "SELECT ano,
  mes,
  id_municipio,
  cnpj_basico,
  instituicao,
  id_verbete,
  valor
  FROM basedosdados.br_bcb_estban.municipio
"
  
  query_b <- paste0('WHERE (ano = ',ano, ')')
  
  query_final = paste(query_a, query_b, sep=" ")
  
  write_parquet(read_sql(query_final),paste0("estban_",ano,".parquet"))
  
  gc()
  
  
  print(ano)
  beep()
  
}


#############################
#PIB Municipal
#############################

query <- "SELECT *
FROM basedosdados.br_ibge_pib.municipio 
WHERE ano IN (2005,2006,2007, 2008, 2009, 2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
"
write_parquet(read_sql(query),"pib_mun.parquet")
gc()
beep()


#############################
#CNPJ/MEI
#############################

query <- "SELECT id_municipio, data_opcao_mei, data_exclusao_mei, COUNT(id_municipio) FROM
	(SELECT cnpj_basico, data_opcao_mei, data_exclusao_mei 
	FROM `basedosdados.br_me_cnpj.simples`
	)  a
LEFT JOIN 
	(SELECT cnpj_basico, id_municipio 
	FROM `basedosdados.br_me_cnpj.estabelecimentos`
	)  b
ON a.cnpj_basico = b.cnpj_basico
WHERE a. data_opcao_mei IS NOT NULL
GROUP BY id_municipio, data_opcao_mei, data_exclusao_mei
"
write_parquet(read_sql(query),"cnpj_mei.parquet")
gc()
beep()

