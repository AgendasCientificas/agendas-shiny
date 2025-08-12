library(readr)
library(shiny)
library(leaflet)
library(dplyr)
library(ggmap)
library(osmdata)
library(ggplot2)
library(geoAr)

conicet <- read_csv("data/raw_data.csv")

conicet = conicet %>%
  mutate(LOCALIDAD = case_when(
    LOCALIDAD == "SAN MARTIN" ~ "SAN MARTIN",
    LOCALIDAD == "S.S. DE JUJUY" ~ "JUJUY",
    LOCALIDAD == "SAN SALVADOR DE JUJUY" ~ "JUJUY",
    LOCALIDAD == "SAN CARLOS DE BARILOCHE" ~ "BARILOCHE",
    LOCALIDAD == "SAN MIGUEL DE TUCUMAN" ~ "TUCUMAN",
    LOCALIDAD == "HURLINGAM" ~ "HURLINGHAM",
    LOCALIDAD == "ESPLUGUES DE LLOBREGAT" ~ "BARCELONA",
    LOCALIDAD == "SAN FDO DEL VALLE DE CATAMARCA" ~ "SAN FERNANDO DEL VALLE DE CATAMARCA",
    LOCALIDAD == "SALTA" ~ "SALTA CAPITAL",
    TRUE ~ LOCALIDAD
  ))

df_coordenadas = conicet %>% 
  # head(5) %>% 
  select(LOCALIDAD, PROVINCIA) %>% 
  distinct_all() %>% 
  mutate(LOCALIDAD = toupper(LOCALIDAD)) %>% 
  rowwise() %>% 
  mutate(coordenadas = list(
    tryCatch(
      getbb(paste(LOCALIDAD, PROVINCIA, "Argentina")),
      error = function(e) NA
      )
    )
  )%>% 
    # getbb(paste(LOCALIDAD, PROVINCIA, "Argentina")))) %>% 
  mutate(lon = if (is.matrix(coordenadas)) mean(coordenadas["x", ]) else NA_real_,
         lat = if (is.matrix(coordenadas)) mean(coordenadas["y", ]) else NA_real_
         ) %>% 
  select(-coordenadas)


sum(is.na(df_coordenadas$lon))

conicet = conicet %>% 
  left_join(df_coordenadas, by = c("LOCALIDAD", "PROVINCIA"))



# Contar cuántas veces aparece cada localidad
conicet <- conicet %>%
  group_by(LOCALIDAD) %>%
  mutate(count = n()) %>%  # Agregar una nueva columna 'count' con el número de filas por localidad
  ungroup() %>%
  mutate(log_count = log10(count)) # o log para base natural # Escalar el conteo usando logaritmo

# Agregar una nueva columna 'REGION' a la base de datos 'conicet'
# Lista de partidos del AMBA


conicet <- conicet %>%
  
  mutate(REGION = case_when(
    
    PROVINCIA == "CABA" ~ "CABA",
    
    PROVINCIA == "BUENOS AIRES" ~ "Buenos Aires",
    
    PROVINCIA %in% c("CORDOBA", "SANTA FE", "ENTRE RIOS") ~ "Región Centro",
    
    PROVINCIA %in% c("MENDOZA", "SAN JUAN", "SAN LUIS") ~ "Región Cuyo",
    
    PROVINCIA %in% c("JUJUY", "SALTA", "TUCUMAN", "CATAMARCA",
                     "SANTIAGO DEL ESTERO", "LA RIOJA") ~ "NOA",
    
    PROVINCIA %in% c("CHACO", "CORRIENTES", "FORMOSA", "MISIONES") ~ "NEA",
    
    PROVINCIA %in% c("LA PAMPA", "RIO NEGRO", "NEUQUEN", "CHUBUT",
                     "SANTA CRUZ", "TIERRA DEL FUEGO") ~ "Patagonia",
    
    TRUE ~ "Otra región"
    
    
  ))


# Agregar una nueva columna 'Nombre_comision' a 'conicet'

conicet <- conicet %>% 
  mutate(Nombre_comision = case_when(
    
    DISCIPLINA.CODIGO == "KA4" ~ "Informática",
    DISCIPLINA.CODIGO == "KB1" ~ "Medicina",
    DISCIPLINA.CODIGO == "KB2" ~ "Biología",
    DISCIPLINA.CODIGO == "KB3" ~ "Bioquímica",
    DISCIPLINA.CODIGO == "KS7" ~ "Psicología y Ciencias de la Educación",
    DISCIPLINA.CODIGO == "KS8" ~ "Antropología Biológica",
    DISCIPLINA.CODIGO == "KS9" ~ "Ciencias Antropológicas"
    
  ))

# # Filtra los datos excluyendo el año 2021
# conicet <- conicet %>%
#   filter(AÑO != 2021)  

# Asegurarse de que no haya valores NA en las columnas 'AÑO' y 'TIPO.CONVOCATORIA'
conicet <- conicet %>%
  filter(!is.na(AÑO), !is.na(TIPO.CONVOCATORIA),
         # !is.na(lon), 
         # !is.na(lat), 
         !is.na(Nombre_comision))

# Suponiendo que 'conicet' tiene una columna 'TIPO' para el tipo de proyecto y 'AÑO' para el año.
# Asegurarse de que no haya valores NA en las columnas 'AÑO' y 'TIPO'
 # Filtrar filas sin valores en 'AÑO' o 'TIPO'
# write.csv(conicet, "D:/concurso_contar_con_datos/conicet_preprocesado.csv", row.names = FALSE) #ubicación Flor
#write.csv(conicet, "C:/Users/usuario/Desktop/Concurso_Contar_con_Datos/conicet_preprocesado.csv", row.names = FALSE) # Fer
write.csv(conicet, "data/processed_data.csv", row.names = F)

