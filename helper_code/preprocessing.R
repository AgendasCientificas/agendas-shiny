library(readr)
library(shiny)
library(leaflet)
library(dplyr)
library(ggmap)
library(ggplot2)
library(geoAr)
library(sf)

conicet <- read_csv("data/raw_data.csv")%>% 
  dplyr::distinct(CONVOCATORIA, NOMBRE.POSTULANTE, AÑO, .keep_all = T)

codigo_deptos <- show_arg_codes(nivel = "departamentos")

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
    # LOCALIDAD == "SANTA FE" ~ "SANTA FE CAPITAL",
    LOCALIDAD == "SAN MARTIN/JOSE LEON SUAREZ" ~ "SAN MARTIN",
    
    TRUE ~ LOCALIDAD
  ))

# 1. Obtener partidos por provincia
provincias <- unique(conicet$PROVINCIA)


df_coordenadas = conicet %>%
  # head(5) %>%
  select(LOCALIDAD, PROVINCIA) %>%
  distinct_all() %>%
  mutate(LOCALIDAD = toupper(LOCALIDAD)) %>%
  rowwise() %>%
  mutate(
    coordenadas = list(
      suppressWarnings(
        tryCatch(
          {
            res <- get_localidades(nombre = LOCALIDAD, provincia = PROVINCIA, max = 1)
            if (nrow(res) == 0) stop("Municipio no encontrado")
            res
          },
          error = function(e) {
            tryCatch(
              get_provincias(nombre = PROVINCIA),
              error = function(e2) NA
            )
          }
        )
      )
    )
  ) %>%
  unnest(coordenadas,keep_empty = T) %>% 
  select(LOCALIDAD, PROVINCIA, centroide_lat, centroide_lon) %>% 
  rename(lon = "centroide_lon",
         lat = "centroide_lat")




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

