
# Descargar y cargar el modelo de lematización para español
modelo_espanol <- udpipe_download_model(language = "spanish")
modelo <- udpipe_load_model(modelo_espanol$file_model)


# Función para lematizar todas las palabras clave

eliminar_tildes <- function(texto) {
  texto <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", texto)  # Eliminar tildes
  return(texto)
}



lematiza_udpipe <- function(frase) {
  # Procesar la frase usando el modelo
  texto_analizado <- udpipe_annotate(modelo, x = frase)
  texto_analizado <- as.data.frame(texto_analizado)
  
  # Retornar solo las palabras lematizadas
  return(texto_analizado$lemma)
}
# Convertir las palabras clave a minúsculas y eliminar tildes
palabras_clave  <- conicet$PALABRAS.CLAVE.PROYECTO %>%
  tolower() %>%               # Convertir todo a minúsculas
  eliminar_tildes()           # Eliminar tildes
texto_completo <- paste(na.omit(palabras_clave), collapse = " ")
texto_completo <- lematiza_udpipe(texto_completo)
# Agrupar las palabras manualmente 
texto_completo <- gsub("niña|niño", "niñez", texto_completo)
# Agrupar las palabras relacionadas antes de contar las frecuencias
texto_completo <- gsub("adolescent|adolescente", "adolescencia", texto_completo)
texto_completo <- gsub("educacional|educativa|educativo", "educacion", texto_completo)
texto_completo <- gsub("alcoholismo|alcoholico", "alcohol", texto_completo)
texto_completo <- gsub("respiratoria", "respiratorio", texto_completo)
texto_completo <- gsub("familiar", "familia", texto_completo)
texto_completo

eliminar <- c("|", "el", "en", "a", "de", "por", "para", NA, "t", "y", ")", "(",
              "b","1", "2", "3","4",  "d", "c", "no", ",", ".","-", "con")

# Filtrar las palabras de 'texto_completo' que no estén en 'eliminar'
texto_completo <- texto_completo[!texto_completo %in% eliminar]

# Convertir a data.frame para mejor visualización
frecuencia_df <- as.data.frame(table(texto_completo))
frecuencia_df$texto_completo <- as.character(frecuencia_df$texto_completo)

# Ver la tabla de frecuencias
colnames(frecuencia_df) <- c("palabra", "freq")