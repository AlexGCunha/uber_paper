rais_path = "C:/Users/xande/OneDrive/Documentos/Doutorado/Research/RAIS"
rais_nova = read_parquet(paste0(rais_path, "/rais_2010_teste.parquet"))
cnova = colnames(rais_nova)
rais_antiga = read_parquet(paste0(rais_path, "/rais_2010.parquet"))
cantiga = colnames(rais_antiga)

rais_antiga = rais_antiga %>% 
  select(-c(tamanho_estabelecimento, tipo_estabelecimento))


write_parquet(rais_nova, "../data/testenova.parquet")
write_parquet(rais_antiga, "../data/testeantiga.parquet")
