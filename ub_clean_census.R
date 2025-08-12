########################################
#This code will:
#- Create some variables at the region level based on 2010 census
#- calculate mean wages by occupation
#- calculate share of college workers by sector
########################################

library(tidyverse)
library(arrow)
library(readxl)
library(fixest)

########################
#2010
#######################
df = read_parquet("../data/census_pes_2010.parquet")


#Define municipality
df = df %>% 
  mutate(munic = substr(munic, 1, 6))

#Define PEA
df = df %>% 
  #Employed will be individuals who worked even if they did not receive in the ref week
  mutate(employed = ifelse(worked_ref_week==1|npaid1==1|npaid2==1|npaid3==1,1,0)) %>% 
  #In PEA will be individuals who are either employed or searched for a job
  mutate(in_pea = ifelse((employed==1|tried_job==1)&age>=16,1,0)) 
gc()



#Formality Status
df = df %>% 
  #InformalC will be employees in the private sector without a signed booklet
  mutate(informal = case_when(employed ==1 & position %in% c(4)  ~ 1,
                              is.na(employed)|is.na(position) ~ NA_real_,
                              T ~0)) %>%
  #Formal will be everyone else that is employed
  mutate(formal = 1-informal*employed) %>% 
  #Self employed
  mutate(self_emp = ifelse(position==5,1,0))

#Instruction level
df = df %>% 
  mutate(
    lths = ifelse(instruct_level <= 2, 1, 0),
    hs_some_college = ifelse(instruct_level == 3, 1, 0),
    college_more = ifelse(instruct_level == 4, 1, 0))


#Income
df = df %>% 
  mutate_at(c('inc_main_job','value_other_income'),~ifelse(is.na(.),0,.)) %>% 
  mutate(wage_total = inc_main_job+value_other_income) 
gc()

##############################
#Estimate share of high skill workers by sector
##############################
alt = data.table(df)
#filter employed
alt = alt[employed == 1]

#define 2-digit cnae
alt[, cnae := fcase(nchar(sector) == 5, substr(sector,1,2),
                    nchar(sector) == 4, paste0(0, substr(sector,1,1)),
                    default = NA)]

#correct only sector in which 2010 census does not correspond to 2.0 cnae
alt[, cnae := fcase(cnae == "48", "46",
                    default = cnae)]

#count workers with college degree by sector
alt = alt[, .(share_col = sum(college_more)/sum(employed)), by = 'cnae']
alt = alt[!is.na(cnae)]

#define sectors in bottom, mid and top share of workers with a college degree
quants = quantile(alt$share_col, c(0.33, 0.66))
alt[, cat_college := fcase(share_col <= quants[1], 1,
                           share_col > quants[1] & share_col <= quants[2], 2,
                           default = 3)]
write_parquet(alt, '../data/share_college_cnae.parquet')
rm(alt, quants)

##############################
#Calculate mean wages by occupation
##############################
#first, we must correct the number of digits in the cbo
df = df %>% 
  mutate(nc = nchar(occupation_cbo),
         cbo_correct = case_when(nc == 3 ~ paste0("0", occupation_cbo),
                                 TRUE ~ occupation_cbo))

#now define 2 and 3-digits occupation
df = df %>% 
  mutate(cbo_2dig = substr(cbo_correct, 1,2),
         cbo_3dig = substr(cbo_correct,1,3))

cbo3 = df %>% 
  filter(employed == 1) %>% 
  group_by(cbo_3dig) %>% 
  summarise(mean_wage_3dig = weighted.mean(inc_main_job, weight)) %>% 
  ungroup()

#define quartile of occupation wage
quants = quantile(cbo3$mean_wage_3dig, c(0.25, 0.50, 0.75))
cbo3 = cbo3 %>% 
  mutate(rank_wage_cbo = case_when(
    mean_wage_3dig <= quants[1] ~ 4, 
    mean_wage_3dig > quants[1] & mean_wage_3dig <= quants[2] ~ 3,
    mean_wage_3dig > quants[2] & mean_wage_3dig <= quants[3] ~ 2,
    TRUE ~ 1))

write_parquet(cbo3, "../data/cbo_3digs.parquet")

#adicionar dados de mmc
micro = read_excel('../data/regioes_geograficas.xlsx') %>% data.table()
micro = micro[, .(CD_GEOCODI, cod_rgi, nome_mun)]
colnames(micro) = c("id_municipio", 'rgi', 'nome_mun')
micro[, `:=`(id_municipio = as.integer(id_municipio),
             rgi = as.integer(rgi))]
micro[, munic := substr(id_municipio, 1, 6)]
micro[, id_municipio := NULL]

df = df %>% 
  left_join(micro, by = 'munic')


#aggregate at microrregion level
agg = df %>% 
  group_by(rgi) %>% 
  summarise(pop_r = sum(weight),
            employed_r = sum(employed * weight, na.rm = T),
            informal_r = sum(informal * weight, na.rm = T),
            pea_r = sum(in_pea*weight, na.rm = T),
            tot_income_r = sum(wage_total*weight, na.rm = T),
            lths_r = sum(lths*weight, na.rm = T),
            hs_some_college_r = sum(hs_some_college * weight, na.rm = T),
            college_more_r = sum(college_more * weight, na.rm = T),
            aux_age = sum(age * in_pea * weight)) %>% 
  ungroup()

#additional variable creation
agg = agg %>% 
  mutate(inf_rate_r = informal_r/employed_r,
         unem_rate_r = 1 - employed_r/pea_r,
         lths_rate_r = lths_r/pop_r,
         hs_rate_r = hs_some_college_r/pop_r,
         college_rate_r = college_more_r/pop_r,
         mean_income_r = tot_income_r/employed_r,
         age_r = aux_age/pea_r)


write_parquet(agg,"../data/rgi_data_10.parquet")
rm(list = ls())
gc()



