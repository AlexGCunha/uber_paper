#tema padrão para g'raficos
tema_padrao = theme_minimal()+
  theme(text = element_text(family = 'serif', size = 12),
        axis.text = element_text(, size = 10),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = 'none')


#regressao
regressao_cs = function(data = df, variavel_dependente, dep_em_log = 0,
                        dep_em_relativo_emp =0,
                        dep_em_relativo_pea = 0, 
                        dep_relativo_2014 = 0,
                        controles_use = sem_controles, 
                        control_group = "nevertreated",
                        base_period = "varying",
                        est_method = 'dr', 
                        control18 =0){
  df_use = copy(data)
  set.seed(456)
  
  if(dep_em_log ==1){
    variaveis = c(variavel_dependente)
    df_use[, (variaveis) := lapply(.SD, function(x) x= log(1 + x)),
           .SDcols = variaveis]
  }
  
  if(dep_em_relativo_pea == 1){
    variaveis = c(variavel_dependente)
    df_use[, (variaveis) := lapply(.SD, function(x) x= x/pea_m),
           .SDcols = variaveis]
  }
  
  if(dep_em_relativo_emp == 1){
    variaveis = c(variavel_dependente)
    df_use[, (variaveis) := lapply(.SD, function(x) x = x/emprego_privado),
           .SDcols = variaveis]
  }
  
  if(dep_relativo_2014 == 1){
    variaveis = c(variavel_dependente)
    df_use = df_use %>% 
      group_by(rgi) %>% 
      mutate(across(variaveis, ~./.[anosem==20142])) %>% 
      ungroup() %>% 
      data.table()
  }
  
  if(control18 == 1){
    df_use = df_use[tem_uber == 1 & anosem <= 20181]
  }
  
  
  
  m1 = att_gt(yname = variavel_dependente,
              tname = "anosem_did",
              gname = "semestre_entrada_did",
              idname = "rgi",
              xformla = controles_use,
              clustervars = c('rgi'),
              control_group = control_group,
              base_period = base_period,
              est_method = est_method,
              data = df_use, 
              pl = TRUE)
  
  m1_agg = aggte(m1, type = "dynamic", min_e = -8,max_e = 3, na.rm = TRUE)
  return(list(m1, m1_agg))
  
}




#Plot Function
plot_es = function(model, title = "", lim_y = c(-0.2, 0.2)){
  att = model[[2]]$overall.att
  se = model[[2]]$overall.se
  att_abs = abs(att)
  p = (1- pnorm(att_abs/se))*2
  #Guarantee p is rounded to 3 decimals
  p = as.character(round(p, 3))
  #I want to appear all 3 decimals, even if they are "0"
  if(p == 0){
    p = "0.000"
  } else{
    p = paste0(p, "00000")
    p = substr(p, 1, 5)
  }
  
  
  
  if(abs(att) - 1.96*se > 0){
    message = paste0("ATT: ", round(att,3), "*", " (", p, ")")
  } else{
    message = paste0("ATT: ", round(att,3 ), " (", p, ")")
  }
  
  #define position:
  max_estimate = max(model[[2]]$att.egt)
  max_se = max(model[[2]]$se.egt)
  position = lim_y[2] - 0.01
  
  plot = ggdid(model[[2]], title = title)+
    annotate("text", x = -8, y = position,label = message, hjust = 0, size= 3.5)+
    ylim(lim_y[1], lim_y[2])+
    tema_padrao+
    scale_color_brewer(palette = 'Dark2')
}
