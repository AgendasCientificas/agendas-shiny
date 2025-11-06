library(stringr)

conicet <- read.csv("data/processed_data.csv") 

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



normalizar_keywords <- function(datos){
  

  
keyword_tokens <- datos_conicet %>%                                       
    mutate(palabras_clave = str_split(PALABRAS.CLAVE.PROYECTO,            # Divido las kw por la coma o la pleca
                                      pattern = "[,|\\|]+")               # (algunas dividen por una y otras por la otra)
    )%>%  
    
    unnest(palabras_clave) %>% 
    mutate(palabras_clave = palabras_clave %>%   
             str_squish() %>%                            # Saco espacios de más
             tolower() %>%                              # Convertir todo a minúsculas
             eliminar_tildes()                           # Eliminar tildes
    ) %>% 
  filter(!is.na(palabras_clave
                ))


tokenized_keywords_df <- keyword_tokens %>% 
  rowwise() %>% 
  mutate(tokenized_keywords = list(lematiza_udpipe(palabras_clave))) %>% 
  unnest(tokenized_keywords)


# Filtrar las palabras de 'texto_completo' que no estén en 'eliminar'

eliminar <- c("|", "el", "en", "a", "de", "por", "para", NA, "t", "y", ")", "(",
              "b","1", "2", "3","4",  "d", "c", "no", ",", ".","-", "con", "na",
              quanteda::stopwords(language = "es")
              )

tokenized_keywords_df = tokenized_keywords_df %>% 
  filter(!tokenized_keywords %in% eliminar)
  
# tokenized_keywords <- tokenized_keywords[!tokenized_keywords %in% eliminar]

# Agrupar las palabras relacionadas antes de contar las frecuencias

tokenized_keywords_df$tokenized_keywords <- gsub("niño|niña|niñez", "niñez", tokenized_keywords_df$tokenized_keywords)
tokenized_keywords_df$tokenized_keywords <- gsub("adolescent|adolescente", "adolescencia", tokenized_keywords_df$tokenized_keywords)
tokenized_keywords_df$tokenized_keywords <- gsub("escuela|escolar", "escuela", tokenized_keywords_df$tokenized_keywords)
tokenized_keywords_df$tokenized_keywords <- gsub("educacional|educativa|educativo", "educacion", tokenized_keywords_df$tokenized_keywords)
tokenized_keywords_df$tokenized_keywords <- gsub("alcoholismo|alcoholico", "alcohol", tokenized_keywords_df$tokenized_keywords)
tokenized_keywords_df$tokenized_keywords <- gsub("respiratoria", "respiratorio", tokenized_keywords_df$tokenized_keywords)
tokenized_keywords_df$tokenized_keywords <- gsub("familiar", "familia", tokenized_keywords_df$tokenized_keywords)
# tokenized_keywords
  
  # # Convertir a data.frame para mejor visualización
  # frecuencia_df <- as.data.frame(table(tokenized_keywords))
  # frecuencia_df$tokenized_keywords <- as.character(frecuencia_df$tokenized_keywords)
  # 
  # # Ver la tabla de frecuencias
  # colnames(frecuencia_df) <- c("palabra", "freq")
  
  return(tokenized_keywords_df)
  
}

freqs <- normalizar_keywords(conicet)


write.csv(freqs, "data/keywords_data.csv", row.names = F)

