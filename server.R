# Cargar las librerías necesarias
library(readr)
library(tidyr)
library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud2)
library(wesanderson)
library(shinyWidgets)
library(geoAr) 
library(reactable)
library(plotly)
library(textstem)
library(udpipe)
source("helper_code/paletas_colores.R")
source("helper_code/lemmatization.R")


# Esta ruta es una ruta relativa que funciona igual en cualquier compu (o debería ja)

conicet <- read.csv("data/processed_data.csv") 



# Asegurarse de que no haya valores NA en las columnas 'AÑO' y 'TIPO.CONVOCATORIA'
conicet <- conicet %>%
  filter(!is.na(AÑO), !is.na(TIPO.CONVOCATORIA), !is.na(lon), !is.na(lat), !is.na(Nombre_comision))

# Reemplazar nombres de regiones según tu requerimiento
# conicet$region <- ifelse(conicet$region == "Buenos Aires", "Buenos Aires",
#                          ifelse(conicet$region == "CABA", "CABA",
#                                 ifelse(conicet$region == "Resto del país", "Resto del país", "Desconocida")))


# conicet <- conicet %>%
#   mutate(region = case_when(
#     region == "Buenos Aires" ~ "Buenos Aires",
#     region == "CABA" ~ "CABA",
#     region == "Región Centro" ~ "Región Centro",
#     region == "Región Cuyo" ~ "Región Cuyo",
#     region == "NOA" ~ "NOA",
#     region == "NEA" ~ "NEA",
#     region == "Patagonia" ~ "Patagonia",
#     TRUE ~ "Desconocida"
#   ))

# Obtener el segundo color de la paleta "Darjeeling2"
color_fondo_titulo <- wes_palette("Darjeeling2")[2]

# Server---------------
server <- function(input, output, session) {
  
  
  
  # Filtrar los datos por año, tipo de proyecto y disciplina
  filteredData <- reactive({
    conicet %>%
      filter(AÑO >= input$yearInput[1], AÑO <= input$yearInput[2],  # Ajustar filtro de año para rango
             Nombre_comision %in% input$disciplinaInput)
    # REGION != "Desconocida")  # Excluir "Desconocida"
  })
  
  output$mapa <- renderLeaflet({
    mapa_argentina <- get_geo("ARGENTINA", level = "provincia")
    
    if (!is.null(mapa_argentina)) {
      leaflet() %>%
        addPolygons(data = mapa_argentina, fillColor = "blue", color = "white") %>%
        geoAr::addArgTiles() %>%
        setView(lng = -63.6152, lat = -38.4161, zoom = 4) %>%
        setMaxBounds(lng1 = -75.0, lat1 = -56.0, lng2 = -45.0, lat2 = -20.0)
    } else {
      leaflet() %>%
        setView(lng = -63.6152, lat = -38.4161, zoom = 4) %>%
        addTiles()
    }
  })
  
  # Observación para actualizar el mapa según los datos filtrados
  observe({
    
    # source("helper_code/paletas_colores.R")
    
    
    # datos <- filteredData()
    datos <- filteredData() %>%
      group_by(LOCALIDAD, PROVINCIA, REGION, lon, lat) %>%
      summarise(count = n(), .groups = "drop")
    
    # if (nrow(datos) > 0) {
    # Calcular el número total de proyectos después de aplicar filtros
    total_proyectos <- sum(datos$count)  # Asegúrate de que 'count' es la columna que contiene los proyectos.
    
    leafletProxy("mapa", data = datos) %>%
      clearMarkers() %>%
      addCircleMarkers(
        ~lon, ~lat, 
        popup = ~paste(LOCALIDAD, "<br>", "Número de proyectos: ", count, "<br>"),  
        radius = ~log10(count + 1) * 5,  # Ajustar el tamaño según los filtros
        color = ~paleta_colores_region(REGION),        # Colores según región
        opacity = 0.8,
        fillOpacity = 1
      )
    # }
  })
  
  
  output$plot_palabras_clave <- renderUI({
  

  
  # Filtrar las 20 palabras más frecuentes
  nube_filtrada <- head(frecuencia_df[order(frecuencia_df$freq, decreasing = TRUE), ], 20)
  
  # Definir la paleta de colores (de oscuro a claro)
  colores <- c("#f39c12", "#e67e22", "#d35400", "#e74c3c", "#c0392b")
  
  # Normalizar las frecuencias para asignar un color
  max_freq <- max(nube_filtrada$freq)
  min_freq <- min(nube_filtrada$freq)
  
  nube_filtrada$color <- sapply(nube_filtrada$freq, function(x) {
    color_idx <- floor((x - min_freq) / (max_freq - min_freq) * (length(colores) - 1)) + 1
    colores[color_idx]
  })
  
  # Crear el gráfico de barras horizontal con Plotly
  barplot_output <- plot_ly(nube_filtrada, x = ~freq, 
                            y = ~reorder(palabra, freq),  # Reordenar palabras por frecuencia
                            type = "bar", orientation = "h",
                            marker = list(color = nube_filtrada$color)) %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "", tickfont = list(size = 16), standoff = 20, automargin = TRUE),
      title = list(
        text = "Palabras clave más frecuentes", 
        font = list(family = "Arial", size = 18, color = "black", weight = "bold"),  
        x = 0.5,  
        y = 0.99  # Centrar el título
      ),
      margin = list(l = 150),  # Aumentar margen izquierdo para separar más las palabras del eje
      height = 700
    )
  
  })
  
  
  # Renderizar gráfico de "Proyectos a lo largo del tiempo"
  output$graficoProyectosTiempo <- renderPlot({
    # Filtrar datos por provincias seleccionadas, considerando "Todas"
    provincias_seleccionadas <- if ("Todas" %in% input$provincia_filter) {
      unique(conicet$PROVINCIA)  # Si "Todas" está seleccionada, incluye todas las provincias
    } else {
      input$provincia_filter  # O incluye las provincias seleccionadas
    }
    
    datos_tiempo <- filteredData() %>%
      filter(PROVINCIA %in% provincias_seleccionadas) %>%
      group_by(AÑO) %>%
      summarise(total_proyectos = n())
    
    max_y <- max(datos_tiempo$total_proyectos, na.rm = TRUE)  # Obtener el valor máximo
    
    ggplot(datos_tiempo, aes(x = AÑO, y = total_proyectos)) +
      geom_line(color = "blue", size = 1) +
      geom_point(size = 3, color = "blue") +
      labs(title = "Proyectos por año", x = "", y = "") +  # Título del gráfico
      scale_x_continuous(breaks = seq(min(datos_tiempo$AÑO), max(datos_tiempo$AÑO), by = 2)) +  # Mostrar cada dos años
      scale_y_continuous(limits = c(0, max_y)) +  # Limitar el eje Y
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18,face = "bold", hjust = 0.5),  # Ajustar tamaño del título
        axis.text.x = element_text(size = 12),  # Ajustar tamaño de fuente en eje X
        axis.text.y = element_text(size = 12)   # Ajustar tamaño de fuente en eje Y
      ) +
      geom_text(aes(label = total_proyectos), 
                nudge_y = -8,  # Mueve las etiquetas hacia arriba (ajusta el valor según tu gráfico)
                vjust = -0.5, 
                size = 4, 
                show.legend = FALSE)  # Etiquetas de puntos
  })
  
  # Renderizar gráfico de "Proyectos por región"
  output$graficoProyectosRegion <- renderPlot({
    datos_region <- filteredData() %>%
      group_by(REGION) %>%
      summarise(total_proyectos = n())
    
    # Reordenar las regiones para que queden descendentes
    # datos_region <- datos_region %>%
    #   mutate(region = reorder(region, total_proyectos)) 
    
    datos_region %>%
      mutate(REGION = reorder(REGION, total_proyectos)) %>% 
      ggplot(aes(x = REGION, y = total_proyectos, fill = REGION)) +
      
      geom_bar(stat = "identity") +
      labs(title = "Proyectos por región", x = "", y = "") +  # Quitar nombre ejes y cambiar título
      scale_fill_manual(values = colores_regiones) +
      # scale_fill_manual(values = c("Buenos Aires" = "#00A08A", "CABA" = "#5BBCD6", "Resto del país" = "#F2AD00")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 18,face = "bold", hjust = 0.5),
            axis.text.x = element_text(size = 14),  
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 20, face = "bold"), 
            legend.position = "none"
      )+
      coord_flip()
  })
  
  # Tabla de datos
  output$data <- renderReactable({
    # Configurar las opciones de la tabla
    options(reactable.theme = reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "8px 12px",
      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
      searchInputStyle = list(width = "100%")
    ))
    
    # Configurar el idioma de la tabla
    options(reactable.language = reactableLang(
      searchPlaceholder = "Buscar",
      noData = "No se encontró información",
      pageInfo = "{rowStart} a {rowEnd} de {rows} proyectos",
      pagePrevious = "\u276e",
      pageNext = "\u276f"
    ))
    
    # Renderizar la tabla
    reactable(
      conicet %>% 
        select(CONVOCATORIA, NOMBRE.POSTULANTE, DISCIPLINA.CODIGO,
               LT.POSTULANTE, DIRECTOR, TITULO.PROYECTO, RESUMEN.PROYECTO,
               PALABRAS.CLAVE.PROYECTO, ESPECIALIDAD.REFERIDA, PAIS, REGION, PROVINCIA, LOCALIDAD),
      filterable = TRUE, 
      minRows = 10, 
      searchable = TRUE,
      defaultPageSize = 10,
      bordered = TRUE,
      highlight = TRUE,
      striped = TRUE,
      # Set table height to ensure scrolling and sticky headers
      style = list(height = "calc(105vh - 150px)"),  # Ajustar 150px para otros elementos UI
      defaultColDef = colDef(
        headerStyle = list(
          position = "sticky",
          top = 0,
          zIndex = 1,
          borderBottom = "2px solid #ddd"
        ),
        minWidth = 150
      ),
      columns = list(
        RESUMEN.PROYECTO = colDef(minWidth = 800),
        TITULO.PROYECTO = colDef(minWidth = 200)
      )
    )
  })
}