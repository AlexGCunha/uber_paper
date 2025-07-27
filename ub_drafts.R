teste = df[semestre_entrada_did == 12
           & (anosem_did == 12| anosem_did == 15)]

teste[, crescimento_emprego := log(emprego_privado[anosem_did == 15])
      - log(emprego_privado[anosem_did == 12]), by = 'id_municipio']

teste = teste[anosem_did == 15]

teste2 = teste[, .(id_municipio, emprego_privado, crescimento_emprego)]
