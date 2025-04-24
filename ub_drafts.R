rais_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/RAIS/"

years = 2012:2021
precos = read_excel("../data/deflator_inpc.xlsx", sheet = "anual_junho")
setDT(precos)
precos[, indice :=NULL]

for(y in 2012:2021){
  #Abrir arquivo para esse estado/ano
  filename = paste0(rais_path, "rais_",y,".parquet")
  rais = read_parquet(filename) %>% data.table()
  rais[, natureza_juridica := as.integer(natureza_juridica)]
  
  #corrigir salarios para inflacao (usar deflator de junho de cada ano)
  rais = merge(rais, precos, by = "ano", all.x = TRUE)
  rais[, valor_remuneracao_media := valor_remuneracao_media * deflator_10]
  

  #mudanca: antes considerava emprego privado (e meio periodo) como tipo_vinculo == 10.
  #agora estou considerando todos os empregos
  vinculos_publicos = c(30, 31, 35)
  vinculos_rurais = c(20, 25, 70, 75)
  
  
  ##indicadores de tipo de emprego
  #privado full time sem prazo determinado
  rais[, privado := fcase(!(tipo_vinculo %in% vinculos_publicos) & 
                            !(tipo_vinculo %in% vinculos_rurais) &
                            natureza_juridica >2038, 1, default =0)]
  
  for(s in 1:2){
    a = Sys.time()
    
    ##filtrar pessoas trabalhando nesse trimestre
    mes_aux = s*6
    #definir empregados como todos, com excessão de:
    #-ainda não admitidos
    #-já demitidos
    rais_s = copy(rais)
    rais_s[, empregado := fcase(
      mes_admissao > mes_aux & !is.na(mes_admissao), 0,
      mes_desligamento <= mes_aux & !is.na(mes_desligamento), 0,
      default = 1
    )]
    
    rais_s[, aux_adm := fcase(
      mes_admissao <= mes_aux | is.na(mes_admissao), 1,
      default = 0
    )]
    
    rais_s[, aux_dem := fcase(
      mes_desligamento > mes_aux | is.na(mes_desligamento), 1,
      default = 0)]
    rais_s[, empregado_2 := fifelse(aux_adm == 1 & aux_dem == 1, 1, 0)]
    
    #dropar ainda não foi admitidos
    rais_v = rais[mes_admissao <= mes_aux | is.na(mes_admissao)]
    #dropar já demitidos
    rais_v = rais_v[mes_desligamento > mes_aux | is.na(mes_desligamento)]
    
    
    privado1 = rais_s[privado == 1, 
                      .(emprego_privado = sum(empregado)
                      )]
    privado2 = rais_v[privado == 1, 
                      .(emprego_privado = .N
                      )]
    
    if(privado1 != privado2){
      print(paste0("diferença em ", y, ' semestre ', s))
    }
    
   print(paste0(y, s))
   b = Sys.time()
   print(difftime(b, a, units = "mins"))
  }
  
}

