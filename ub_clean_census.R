########################################
#This code will:
#- Create some variables at the municipality level based on 2010 census
#- calculate mean wages by occupation and residual wages by occupation
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

#calculate mean wages by cbo
cbo2 = df %>% 
  filter(employed == 1) %>% 
  group_by(cbo_2dig) %>% 
  summarise(mean_wage_2dig = weighted.mean(inc_main_job, weight)) %>% 
  ungroup()


cbo3 = df %>% 
  filter(employed == 1) %>% 
  group_by(cbo_3dig) %>% 
  summarise(mean_wage_3dig = weighted.mean(inc_main_job, weight)) %>% 
  ungroup()

write_parquet(cbo2, "../data/cbo_2digs.parquet")
write_parquet(cbo3, "../data/cbo_3digs.parquet")


#aggregate at municipality level
agg = df %>% 
  group_by(munic) %>% 
  summarise(pop_m = sum(weight),
            employed_m = sum(employed * weight, na.rm = T),
            informal_m = sum(informal * weight, na.rm = T),
            pea_m = sum(in_pea*weight, na.rm = T),
            tot_income_m = sum(wage_total*weight, na.rm = T),
            lths_m = sum(lths*weight, na.rm = T),
            hs_some_college_m = sum(hs_some_college * weight, na.rm = T),
            college_more_m = sum(college_more * weight, na.rm = T),
            aux_age = sum(age * in_pea * weight)) %>% 
  ungroup()

#additional variable creation
agg = agg %>% 
  mutate(inf_rate_m = informal_m/employed_m,
         unem_rate_m = 1 - employed_m/pea_m,
         lths_rate_m = lths_m/pop_m,
         hs_rate_m = hs_some_college_m/pop_m,
         college_rate_m = college_more_m/pop_m,
         mean_income_m = tot_income_m/employed_m,
         age_m = aux_age/pea_m)


write_parquet(agg,"../data/munic_data_10.parquet")
rm(list = ls())
gc()



