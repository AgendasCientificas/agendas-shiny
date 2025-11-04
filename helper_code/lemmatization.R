

#### Modelo de lenguaje en español ####

modelo_espanol <- udpipe_download_model(language = "spanish", overwrite = F) # Solo lo descarga si no existe
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


