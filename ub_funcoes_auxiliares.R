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
      group_by(id_municipio) %>% 
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
              idname = "id_municipio",
              xformla = controles_use,
              clustervars = c('id_municipio'),
              control_group = control_group,
              base_period = base_period,
              est_method = est_method,
              data = df_use, 
              pl = TRUE)
  
  m1_agg = aggte(m1, type = "dynamic", min_e = -6,max_e = 5, na.rm = TRUE)
  return(list(m1, m1_agg))
  
}


#Regressao 2WFE
regressao_fe = function(variavel_dependente, 
                        dep_em_log = 0, dep_em_relativo_emp =0,
                        dep_em_relativo_pea = 0, 
                        minimo_cidades = 0, 
                        control18 =0 ){
  df_use = copy(df)
  
  
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
  
  
  #dropar grupos onde o numero de cidades tratadas no periodo é menor que o minimo
  df_use[, conta_cidade_grupo := length(unique(id_municipio)), by = .(semestre_entrada_did)]
  df_use = df_use[conta_cidade_grupo >=minimo_cidades]
  
  #criar indicador de periodos did
  df_use = df_use[tem_uber == 0 |(tem_uber == 1 & !is.na(semestre_entrada))]
  df_use[, uber := fcase(tem_uber == 1 & anosem >= semestre_entrada, 1, 
                         default = 0)]
  df_use[, semestre := as.integer(substr(anosem, 5,5))]
  df_use[, semestre_relativo_did := anosem_did - semestre_entrada_did]
  
  formula = paste0(variavel_dependente, " ~ i(semestre_relativo_did, ref = '-1'):tem_uber |
               anosem + id_municipio ")
  formula = as.formula(formula)
  m1 = feols(formula ,
             data = df_use[(tem_uber == 0 | (tem_uber == 1 & semestre_relativo_did %in% c(-6:5))) ],
             cluster = "id_municipio")
  
  return(m1)
  
}

#Plot Function
plot_es = function(model, title = ""){
  att = unlist(model)$overall.att
  se = unlist(model)$overall.se
  att_abs = abs(att)
  p = (1- pnorm(att_abs/se))*2
  if(abs(att) - 1.96*se > 0){
    message = paste0("ATT: ", round(att,3), "*", " (", round(p,2), ")")
  } else{
    message = paste0("ATT: ", round(att,3 ), " (", round(p,2), ")")
  }
  
  #define position:
  max_estimate = max(model[[2]]$att.egt)
  max_se = max(model[[2]]$se.egt)
  position = max_estimate + 1.5*max_se
  
  plot = ggdid(model[[2]], title = title)+
    coord_cartesian(xlim = c(-6, 5)) + 
    xlim(-6,6)+
    theme(plot.title = element_text(size=8)) +
    annotate("text", x = -6, y = position,label = message, hjust = 0)
}

#plots from twfe estimations
plot_fe = function(model, title = ""){
  info = tidy(model, conf.int = TRUE)
  info = info %>% 
    mutate(period = c(seq(-6, -2, 1), seq(0, 5, 1))) %>% 
    select(period, estimate, conf.low, conf.high)
  
  #add info for baseline period (all 0)
  base = c(-1, 0, 0, 0)
  info = rbind(info, base)
  info = info %>% arrange(period)
  info = info %>% 
    mutate(after = ifelse(period < 0, "pre", "post"))
  
  #plot
  plot = ggplot(info, aes(x = period, y = estimate, color = after))+
    geom_point()+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .1)+
    theme_classic()+ geom_hline(aes(yintercept = 0), linetype ="dashed")+
    labs(title = title, y = "")+theme(legend.position = "none")
  
  return(plot)
  
  
  
  
}


#################################
#Honest Did by Rambachan nad Roth (2022)
#Inplemented by Pedro Santanna
#################################

#' @title honest_did
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021)
honest_did <- function(...) UseMethod("honest_did")

#' @title honest_did.AGGTEobj
#'
#' @description a function to compute a sensitivity analysis
#'  using the approach of Rambachan and Roth (2021) when
#'  the event study is estimating using the `did` package
#'
#' @param e event time to compute the sensitivity analysis for.
#'  The default value is `e=0` corresponding to the "on impact"
#'  effect of participating in the treatment.
#' @param type Options are "smoothness" (which conducts a
#'  sensitivity analysis allowing for violations of linear trends
#'  in pre-treatment periods) or "relative_magnitude" (which
#'  conducts a sensitivity analysis based on the relative magnitudes
#'  of deviations from parallel trends in pre-treatment periods).
#' @inheritParams HonestDiD::createSensitivityResults
#' @inheritParams HonestDid::createSensitivityResults_relativeMagnitudes
honest_did.AGGTEobj <- function(es,
                                e          = 0,
                                type       = c("smoothness", "relative_magnitude"),
                                gridPoints = 100,
                                ...) {
  
  
  
  return(list(robust_ci=robust_ci, orig_ci=orig_ci, type=type))
}
