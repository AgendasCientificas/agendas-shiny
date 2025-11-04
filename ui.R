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
source("helper_code/paletas_colores.R")

# Obtener el segundo color de la paleta "Darjeeling2"
color_fondo_titulo <- wes_palette("Darjeeling2")[2]

# Esta ruta es una ruta relativa que funciona igual en cualquier compu (o debería ja)

conicet <- read.csv("data/processed_data.csv") 

#UI---------------------
ui <- 
  navbarPage("",
             
             tabPanel("Página principal",
                      fluidPage(
                        tags$head(
                          tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
                          tags$style(HTML(paste0("
            body, html { font-family: 'Roboto', sans-serif; }
            .titulo-app {
              background-color: ", color_fondo_titulo, "; 
              color: white;
              font-weight: bold;
              padding: 15px;
              border-radius: 10px;
              text-align: center;
            }
            .full-panel {
              height: 100vh !important;
              overflow-y: auto;
            }
          ")))
                        ),
                        
                        div(
                          class = "titulo-app",
                          h1(
                            style = "margin: 20px 0; font-family: 'Nimbus Sans', sans-serif; font-weight: bold;", 
                            "Agendas científicas sobre desarrollo infantil: 
                            relevamiento y análisis de datos abiertos del CONICET"
                          )
                        ),
                        
                        fluidPage(
                          fluidRow(
                            # Primer recuadro
                            column(4, 
                                   div(
                                     style = 'font-size: 18px; text-align: justify; background-color: #ABDDDE; padding: 10px; border-radius: 5px; margin: 15px 5px;', 
                                     p(HTML("<strong>CONICET</strong> es el principal organismo de ciencia y tecnología del país. 
                                            Conocer las temáticas de investigación de quienes lo integran y su localización geográfica e institucional
                                            constituye información valiosa para la exploración de agendas de investigación, 
                                            el establecimiento de vínculos interinstitucionales y de redes de investigadores. 
                                            Asimismo es un potencial insumo para elaboración de políticas científicas y la 
                                            discusión pública basada en información accesible.")),
                                     p(HTML("<strong>Esta información es pública, pero no es de fácil acceso.</strong>"))
                                   )
                            ),
                            
                            # Segundo y Tercer recuadro unificados
                            column(8, 
                                   div(
                                     style = 'font-size: 18px; text-align: justify; background-color: #ABDDDE; padding: 10px; border-radius: 5px; margin: 15px 5px;', 
                                     p(HTML("Basándonos en las bases de datos recopiladas, analizamos el comportamiento de las becas e ingresos 
                                     otorgados por organismo en el periodo 2010-2020 a candidatos y candidatas con temas vinculados al desarrollo Niños, 
                                     Niñas y Adolescentes considerando asimismo su distribución geográfica y disciplinar. 
                                            Estos resultados fueron publicados en el año 2024 en la revista Iberoamericana de Ciencia, Tecnología y Sociedad (Smulski, et al. 2024).")),
                                     p(HTML("Aquí presentamos un tablero interactivo que pone a disposición la información recopilada, 
                                            sistematizada y analizada en dicha publicación con el fin de facilitar el acceso a la información."))
                                   )
                            )
                          )
                        ),
                        
                        sidebarPanel(
                          class = "full-panel",  # Añadir la clase para altura completa
                          sliderInput("yearInput", "Seleccionar período de tiempo",
                                      min = min(conicet$AÑO, na.rm = TRUE),
                                      max = max(conicet$AÑO, na.rm = TRUE),
                                      value = range(conicet$AÑO, na.rm = TRUE),
                                      sep = ""),
                          
                          shinyWidgets::pickerInput("disciplinaInput", 
                                                    "Seleccionar disciplina:", 
                                                    choices = unique(conicet$Nombre_comision),
                                                    selected = unique(conicet$Nombre_comision),
                                                    options = list(`actions-box` = TRUE),
                                                    multiple = TRUE,
                          ),
                          
                          # Nube de palabras
                          uiOutput("plot_palabras_clave")  # Ajuste de altura sin scroll
                        ),
                        
                        mainPanel(
                          fluidRow(
                            column(6, leafletOutput("mapa", height = "100vh")),  # Mapa a la izquierda
                            column(6, 
                                   fluidRow(
                                     column(12, plotOutput("graficoProyectosRegion", height = "50vh")),  
                                     column(12, 
                                            fluidRow(
                                              column(12, 
                                                     shinyWidgets::pickerInput("provincia_filter", "Seleccione una provincia:",
                                                                               choices = c("Todas", unique(conicet$PROVINCIA)), 
                                                                               selected = "Todas", 
                                                                               options = list(`actions-box` = TRUE), 
                                                                               multiple = TRUE)),  
                                              column(12, plotOutput("graficoProyectosTiempo", height = "50vh"))   
                                            )
                                     )
                                   )
                            )
                          )
                        )
                      ),
                      
                      tags$footer(
                        HTML('<span style="font-size:12px;"><i>Autores: Florencia Altschuler, Federico Giovannetti, Fernando Steeb y Mariana Smulski. </i></span><br>
        <span style="font-size:12px;">Datos obtenidos de <a href="https://ojs.revistacts.net/index.php/CTS/article/view/410" target="_blank";"><i>Smulski, M., Giovannetti, F., Steeb, F., Serra, A. L. P., Grasser, F. B., Jove, G. M., & Cevasco, J. (2024). Agendas científicas sobre desarrollo infantil en CONICET: Evolución de becas e ingresos de investigadores en el periodo 2010-2020. Revista Iberoamericana de Ciencia, Tecnología y Sociedad</i></a> - CTS.</span><br>
        <span style="font-size:12px;">El código fuente de este tablero está disponible en nuestro <a href="https://github.com/AgendasCientificas" target="_blank";">repositorio de GitHub</a>.</span>'),
                        align = "left",
                        style = "width:100%; padding:10px; background-color:#f0f5f9;"
                      )
                      
                      
                      
             ),
             
             tabPanel("Datos",
                      HTML("<h2 style='font-size: 20px; font-weight: bold; color: #046C9A;'>Acá vas a poder acceder a los datos completos...</h2>"),
                      fluidPage(
                        tags$style(HTML("
      .reactable { font-size: 10px; }
    ")),
                        reactableOutput("data")
                      )
             )
  )