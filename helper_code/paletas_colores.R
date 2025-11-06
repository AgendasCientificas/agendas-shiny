
# Obtener el segundo color de la paleta "Darjeeling2"
color_fondo_titulo <- wes_palette("Darjeeling2")[2]

paleta_colores_region <- function(region) {
  colores <- c(
    "CABA" = "#FF6666",
    "Buenos Aires" = "#00A08A",
    "Región Centro" = "#F2AD00",
    "Región Cuyo" = "#F98400",
    "NOA" = "#5BBCD6",
    "NEA" = "#984EA3",
    "Patagonia" = "#377EB8",
    "Desconocida" = "gray"
  )
  
  # Retorná el color según la provincia, o "Desconocida" si no está en el vector
  colores[region] %>% 
    ifelse(is.na(.), colores["Desconocida"], .) %>% 
    as.vector()
  
}

colores <- c("#FF6666", "#00A08A", "#F2AD00",
             "#F98400","#5BBCD6", "#984EA3",
             "#377EB8")

nombres_regiones <- c("CABA", "Buenos Aires", "Región Centro",
                      "Región Cuyo", "NOA", "NEA", "Patagonia")  # Nombres de las regiones

colores_regiones <- setNames(colores,
                             c("CABA", "Buenos Aires",
                               "Región Centro", "Región Cuyo",
                               "NOA", "NEA", "Patagonia")) #Asignar nombres a colores