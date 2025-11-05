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
             
             
#### Pestaña de página principal ####

            tabPanel("Página principal",
                     
                     fluidPage(
                       
                       tags$head(
                          # tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
                          
                          
                          tags$style(
                            HTML("
                @import url('https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100..900;1,100..900&display=swap');
            ")
                          ),
                          
                          ),
                       
                       div(
                         class = "titulo-app",
                         h1(
                           style = "margin: 20px 0; font-weight: bold;",
                           "Agendas científicas sobre desarrollo infantil: 
                           relevamiento y análisis de datos abiertos del CONICET"
                           )
                         ),
                       
                       fluidPage(
                         
                         tags$head(
                           # tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap"),
                           
                           
                           tags$style(
                             HTML("
                @import url('https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100..900;1,100..900&display=swap');
            ")
                           ),
                           
                         ),
                          fluidRow(
                            # Primer recuadro
                            column(4, 
                                   div(
                                     style = 'font-size: 18px; text-align: justify; background-color: #e1f7f7; padding: 10px; border-radius: 5px; margin: 15px 5px;', 
                                     p(HTML("Somos un grupo de investigación perteneciente al <strong>CONICET</strong>, el principal organismo de ciencia y tecnología del país.<br><br>
                                            Nos interesa conocer las temáticas de investigación de quienes lo integran y su localización geográfica e institucional
                                            constituye información valiosa para la exploración de agendas de investigación, 
                                            el establecimiento de vínculos interinstitucionales y de redes de investigadores. 
                                            Asimismo es un potencial insumo para elaboración de políticas científicas y la 
                                            discusión pública basada en información accesible.")),
                                     p(HTML("<strong>Esta información es pública, pero no es de fácil acceso.</strong>"))
                                   )
                            ),
                            
                            # Segundo recuadro
                            column(8, 
                                   div(
                                     style = 'font-size: 18px; text-align: justify; background-color: #e1f7f7; padding: 10px; border-radius: 5px; margin: 15px 5px;', 
                                     p(HTML("Basándonos en las bases de datos recopiladas, analizamos el comportamiento de las becas e ingresos 
                                     otorgados por organismo en el periodo 2010-2020 a candidatos y candidatas con temas vinculados al desarrollo Niños, 
                                     Niñas y Adolescentes considerando asimismo su distribución geográfica y disciplinar. 
                                            Estos resultados fueron publicados en el año 2024 en la revista Iberoamericana de Ciencia, Tecnología y Sociedad (Smulski, et al. 2024).")),
                                     p(HTML("Aquí presentamos un tablero interactivo que pone a disposición la información recopilada, 
                                            sistematizada y analizada en dicha publicación con el fin de facilitar el acceso a la información."))
                                   ),
                                   
                                   div(
                                     style = 'font-size: 18px; text-align: justify; background-color: #f5f5f5; padding: 10px; border-radius: 5px; margin: 15px 5px; font-weight: bold;', 
                                     p(HTML("Seleccionar parámetros de búsqueda")),
                                     
                                     sliderInput("yearInput",
                                                 "Período de tiempo:",
                                                 min = min(conicet$AÑO, na.rm = TRUE),
                                                 max = max(conicet$AÑO, na.rm = TRUE),
                                                 value = range(conicet$AÑO, na.rm = TRUE),
                                                 width = "100%",
                                                 sep = ""),
                                     
                                     shinyWidgets::pickerInput("disciplinaInput", 
                                                               "Disciplina:",
                                                               choices = unique(conicet$Nombre_comision),
                                                               selected = unique(conicet$Nombre_comision),
                                                               options = list(`actions-box` = TRUE),
                                                               multiple = TRUE,
                                                               width = "100%"
                                                               )
                                   

                                   ),
                                   
                                   
                                   
                            )
                          )
                        ),
                       
                       # Panel lateral
                        
                        # sidebarPanel(
                        #   class = "full-panel",  # Añadir la clase para altura completa
                        #   # sliderInput("yearInput", "Seleccionar período de tiempo",
                        #   #             min = min(conicet$AÑO, na.rm = TRUE),
                        #   #             max = max(conicet$AÑO, na.rm = TRUE),
                        #   #             value = range(conicet$AÑO, na.rm = TRUE),
                        #   #             sep = ""),
                        #   
                        # 
                        #   
                        #   # Nube de palabras
                            # Ajuste de altura sin scroll
                        # ),
                        
                        # mainPanel(
                          fluidRow(
                            
                            #Barplot de 
                            column(4, uiOutput("nubePalabras")),
                            
                            column(4, leafletOutput("mapa", height = "100vh")),  # Mapa a la izquierda
                            column(4, 
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
                        # )
                      ),
                     
                     
                     
                     
                      
                      tags$footer(
                        HTML('<span style="font-size:12px;"><i>Autores: Florencia Altschuler, Federico Giovannetti, Fernando Steeb y Mariana Smulski. </i></span><br>
        <span style="font-size:12px;">Datos obtenidos de <a href="https://ojs.revistacts.net/index.php/CTS/article/view/410" target="_blank";"><i>Smulski, M., Giovannetti, F., Steeb, F., Serra, A. L. P., Grasser, F. B., Jove, G. M., & Cevasco, J. (2024). Agendas científicas sobre desarrollo infantil en CONICET: Evolución de becas e ingresos de investigadores en el periodo 2010-2020. Revista Iberoamericana de Ciencia, Tecnología y Sociedad</i></a> - CTS.</span><br>
        <span style="font-size:12px;">El código fuente de este tablero está disponible en nuestro <a href="https://github.com/AgendasCientificas" target="_blank";">repositorio de GitHub</a>.</span>'),
                        align = "left",
                        style = "width:100%; padding:10px; background-color:#f0f5f9;"
                      )
                      
                      
                      
             ),
             
             
             
#### Pestaña de tabla con datos ####
             
             
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