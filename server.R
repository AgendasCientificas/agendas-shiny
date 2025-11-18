

#### Librerías ####

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
library(DT)
library(htmltools)
source("helper_code/paletas_colores.R")


#### Raw data ####

# Esta ruta es una ruta relativa que funciona igual en cualquier compu (o debería ja)

conicet <- read.csv("data/processed_data.csv") 

# Asegurarse de que no haya valores NA en las columnas 'AÑO' y 'TIPO.CONVOCATORIA'
conicet <- conicet %>%
  filter(
    !is.na(AÑO), 
    !is.na(TIPO.CONVOCATORIA), 
    !is.na(lon), 
    !is.na(lat), 
    !is.na(Nombre_comision),
    !is.na(REGION),         
    !is.na(PROVINCIA),      
    REGION != "Otra región" 
  )

#### Server ####

server <- function(input, output, session) {
  
  ##### filteredData ####
  
  # Filtrar los datos por año, tipo de proyecto y disciplina
  filteredData <- reactive({
    conicet %>%
      filter(AÑO >= input$yearInput[1], AÑO <= input$yearInput[2],  # Ajustar filtro de año para rango
             Nombre_comision %in% input$disciplinaInput)
  })
  
  
  #### mapa ####
  
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
    
    datos <- filteredData() %>%
      group_by(LOCALIDAD, PROVINCIA, REGION, lon, lat) %>%
      summarise(count = n(), .groups = "drop")
    

    # Calcular el número total de proyectos después de aplicar filtros
    total_proyectos <- sum(datos$count) 
    
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
  
  
  #### Palabras clave ####
  
  output$plot_palabras_clave <- renderUI({
  
  # Levanto los datos con keywords
  tokenized_keywords_df <- read.csv("data/keywords_data.csv")
  
  # Filtrar los datos por año, tipo de proyecto y disciplina
  filtered_kw <- reactive({
    tokenized_keywords_df %>%
      filter(AÑO >= input$yearInput[1], AÑO <= input$yearInput[2],  # Ajustar filtro de año para rango
             Nombre_comision %in% input$disciplinaInput)
  })
  
  
  # Convertir a data.frame para mejor visualización
  frecuencia_df <- as.data.frame(table(filtered_kw()$tokenized_keywords))

  # Ver la tabla de frecuencias
  colnames(frecuencia_df) <- c("palabra", "freq")
  
  # frecuencia_df <- normalizar_keywords(conicet)

  # Filtrar las 20 palabras más frecuentes
  nube_filtrada <- head(frecuencia_df[order(frecuencia_df$freq, decreasing = TRUE), ], 15)
  
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
    ) %>% 
    config(displayModeBar = FALSE)
  
  })
  
  #### Proyectos tiempo ####
  
  #### Proyectos tiempo ####
  
  output$graficoProyectosTiempo <- renderPlot({
    
    provincias_seleccionadas <- input$provincia_filter
    
    # Si el switch está en "Agrupado (Total)" (TRUE)
    if (input$agrupar_grafico) {
      
      # --- INICIO DE NUEVA LÓGICA DE COLOR ---
      
      # 1. Encontrar las regiones únicas de las provincias seleccionadas
      #    Usamos el 'conicet' cargado al inicio del server
      regiones_seleccionadas <- conicet %>%
        filter(PROVINCIA %in% provincias_seleccionadas) %>%
        select(REGION) %>%
        distinct() %>%
        pull()
      
      n_regiones <- length(regiones_seleccionadas)
      
      # 2. Definir el color
      if (n_regiones == 1) {
        # Si es 1, tomar el color de la paleta 'colores_regiones'
        # (Asumimos que 'colores_regiones' es un vector/lista nombrado, ej: colores_regiones["Patagonia"])
        color_linea <- colores_regiones[regiones_seleccionadas[1]]
      } else {
        # Si son 0 o más de 1, usar un color neutral
        color_linea <- "blue"
      }
      # --- FIN DE LÓGICA DE COLOR ---
      
      
      datos_tiempo <- filteredData() %>%
        filter(PROVINCIA %in% provincias_seleccionadas) %>%
        group_by(AÑO) %>%
        summarise(total_proyectos = n())
      
      max_y <- ifelse(length(datos_tiempo$total_proyectos) > 0, max(datos_tiempo$total_proyectos, na.rm = TRUE), 0)
      
      ggplot(datos_tiempo, aes(x = AÑO, y = total_proyectos)) +
        # --- USAMOS LA VARIABLE DE COLOR ---
        geom_line(color = color_linea, size = 1) +
        geom_point(size = 3, color = color_linea) +
        # ---
        labs(title = "Proyectos por año (Total)", x = "", y = "") + 
        scale_x_continuous(breaks = seq(min(datos_tiempo$AÑO, na.rm=T), max(datos_tiempo$AÑO, na.rm=T), by = 2)) +
        scale_y_continuous(limits = c(0, max_y * 1.1)) + # Damos 10% de espacio
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18,face = "bold", hjust = 0.5), 
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12)  
        ) +
        geom_text(aes(label = total_proyectos), 
                  vjust = -0.5, 
                  size = 4, 
                  show.legend = FALSE) 
      
    } else {
      # Si está en "Por Provincia" (FALSE), creamos el gráfico desagrupado
      # (Esta parte no se modifica)
      
      datos_tiempo <- filteredData() %>%
        filter(PROVINCIA %in% provincias_seleccionadas) %>%
        group_by(AÑO, PROVINCIA) %>% # Agrupamos por PROVINCIA
        summarise(total_proyectos = n(), .groups = "drop")
      
      max_y <- ifelse(length(datos_tiempo$total_proyectos) > 0, max(datos_tiempo$total_proyectos, na.rm = TRUE), 0)
      
      ggplot(datos_tiempo, aes(x = AÑO, y = total_proyectos, color = PROVINCIA, group = PROVINCIA)) + 
        geom_line(size = 1) +
        geom_point(size = 2) + 
        labs(title = "Proyectos por año y provincia", x = "", y = "") + 
        scale_x_continuous(breaks = seq(min(datos_tiempo$AÑO, na.rm=T), max(datos_tiempo$AÑO, na.rm=T), by = 2)) +
        scale_y_continuous(limits = c(0, max_y * 1.1)) + 
        
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18,face = "bold", hjust = 0.5),
          axis.text.x = element_text(size = 12), 
          axis.text.y = element_text(size = 12),
          legend.position = "bottom" 
        )
    }
  })
  
  #### Proyectos región ####
  
  output$graficoProyectosRegion <- renderPlot({
    datos_region <- filteredData() %>%
      group_by(REGION) %>%
      summarise(total_proyectos = n())
    
    datos_region %>%
      mutate(REGION = reorder(REGION, total_proyectos)) %>% 
      ggplot(aes(x = REGION, y = total_proyectos, fill = REGION)) +
      
      geom_bar(stat = "identity") +
      labs(title = "Proyectos por región", x = "", y = "") +  # Quitar nombre ejes y cambiar título
      scale_fill_manual(values = colores_regiones) +
      theme_minimal() +
      theme(plot.title = element_text(size = 18,face = "bold", hjust = 0.5),
            axis.text.x = element_text(size = 14),  
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 20, face = "bold"), 
            legend.position = "none"
      )+
      coord_flip()
  })

  #### Tabla de datos ####
  output$data <- renderReactable({
    
    # input$reset_sort
    
    
    # Configurar las opciones de la tabla
    options(reactable.theme = reactableTheme(
      color = "black",
      headerStyle = list(
        "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
        "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 80%, 96%)"),
        "& .Reactable-header-sort-icon path" = list(fill = "blue"),
        borderColor = "#555",
        color = "black",
        fontWeight = "bold",
        fontSize = 10
      ),
      
      # style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
      searchInputStyle = list(width = "100%",
                              borderColor = "#bababa",
                              color = "black")
    ))
    
    
    # theme = reactableTheme(

    # Configurar el idioma de la tabla
    options(reactable.language = reactableLang(
      searchPlaceholder = "Buscar",
      noData = "No se encontró información",
      pageInfo = "{rowStart} a {rowEnd} de {rows} proyectos",
      pagePrevious = "\u276e",
      pageNext = "\u276f"
    ))
    

    
    # Fuzzy search method based on match-sorter
    # See https://github.com/kentcdodds/match-sorter for advanced customization
    matchSorterSearchMethod <-  JS("function(rows, columnIds, searchValue) {
                                    const keys = columnIds.map(function(id) {
                                    return function(row) {
                                    return row.values[id]
                                    }
                                    })
                                    return matchSorter(rows, searchValue, { keys: keys })
                                    }")
    

      
      reactable(
        conicet %>%
        select(Año = AÑO, Convocatoria = TIPO.CONVOCATORIA, Comisión = Nombre_comision,
               Postulante = NOMBRE.POSTULANTE, 'Lugar de trabajo' = LT.POSTULANTE, 
               'Director/a' = DIRECTOR, 'Título del proyecto' =  TITULO.PROYECTO,
               Resumen = RESUMEN.PROYECTO, 'Palabras clave' = PALABRAS.CLAVE.PROYECTO,
               Especialidad = ESPECIALIDAD.REFERIDA, Región = REGION, Provincia = PROVINCIA, Localidad = LOCALIDAD),
        searchable = TRUE,
        searchMethod = matchSorterSearchMethod,
        defaultPageSize = 5,
        filterable = TRUE, 
        # minRows = 10,
        # bordered = TRUE,
        highlight = TRUE,
        striped = TRUE,
        
        showSortable = TRUE,
            # Inmovilizar header
            style = list(height = "calc(105vh - 150px)"),  # Ajustar 150px para otros elementos UI
            defaultColDef = colDef(
              headerStyle = list(
                position = "sticky",
                top = 0,
                zIndex = 1
              ),
              minWidth = 150
            ),
            columns = list(
              Año = colDef(maxWidth = 50, style = list(fontSize = 12)),
              Convocatoria = colDef(style = list(fontSize = 12)),
              Comisión = colDef(style = list(fontSize = 12), maxWidth = 100),
              # Comisión = colDef(),
              'Lugar de trabajo' = colDef(minWidth = 200),
              Resumen = colDef(minWidth = 800),
              'Título del proyecto' = colDef(minWidth = 200)
            )
      )
      

  })
}

  

  

  
  
  
}
