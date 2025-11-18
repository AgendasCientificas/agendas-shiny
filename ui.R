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
library(DT)
library(htmltools)

source("helper_code/paletas_colores.R")


# Esta ruta es una ruta relativa que funciona igual en cualquier compu (o debería ja)

conicet <- read.csv("data/processed_data.csv") 

conicet <- conicet %>%
  filter(
    !is.na(REGION), 
    !is.na(PROVINCIA), 
    REGION != "Otra región"
  )

# 1. Crear el mapa de Región -> Provincia
prov_region_map <- conicet %>%
  select(REGION, PROVINCIA) %>%
  distinct() %>%
  arrange(REGION, PROVINCIA)

# 2. Crear la lista agrupada (nombrada) para el pickerInput
lista_provincias_agrupada <- split(prov_region_map$PROVINCIA, prov_region_map$REGION)
# match-sorter library dependency. Include this anywhere in your document or app.
matchSorterDep <- htmlDependency(
  "match-sorter", 
  "1.8.0",
  c(href = "https://unpkg.com/match-sorter@1.8.0/dist/umd/"),
  script = "match-sorter.min.js"
)

#UI---------------------
ui <- 
  navbarPage("",
             
             
             #### Pestaña de página principal ####
             
             tabPanel("Página principal",
                      
                      fluidPage(
                        
                        tags$head(
                          
                          tags$style(
                            HTML("
                            @import url('https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100..900;1,100..900&display=swap');
                             
                            body {
                              font-family: 'Montserrat', sans-serif;
                            }"
                            )
                          )
                          
                        ),
                        
                        div(
                          
                          class = "titulo-app",
                          
                          h1(
                            style = "margin: 20px 0; font-weight: bold; text-align: center;",
                            HTML("Agendas científicas sobre desarrollo infantil:
                                <br>
                                relevamiento y análisis de datos abiertos del CONICET")
                          )
                          
                        ),
                        
                        fluidPage(
                          
                          tags$head(
                            
                            tags$style(
                              
                              HTML("
                              @import url('https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100..900;1,100..900&display=swap');
                                 ")
                            ),
                          ),
                          
                          fluidRow(
                            # style = "display: flex; flex-wrap: wrap;",
                            
                            # Primer recuadro
                            column(4, 
                                   div(
                                     style = 'height: 90%;font-size: 18px; text-align: justify; background-color: #e1f7f7; padding: 10px; border-radius: 5px; margin: 15px 5px;', 
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
                                   # style = "display: flex; flex-direction: column;",
                                   
                                   fluidRow(
                                     
                                     
                                     
                                     div(
                                       style = 'height: 100%;font-size: 18px; text-align: justify; background-color: #e1f7f7; padding: 10px; border-radius: 5px; margin: 15px 5px;', 
                                       p(HTML("Basándonos en bases de datos recopiladas a partir de pedidos de Acceso a la Información, analizamos el comportamiento de las becas e ingresos 
                                     otorgados por organismo en el periodo 2010-2020 a candidatos y candidatas con temas vinculados al desarrollo Niños, 
                                     Niñas y Adolescentes considerando asimismo su distribución geográfica y disciplinar. 
                                            Estos resultados fueron publicados en el año 2024 en la revista Iberoamericana de Ciencia, Tecnología y Sociedad (Smulski, et al. 2024).")),
                                       p(HTML("Aquí presentamos un tablero interactivo que pone a disposición la información recopilada, 
                                            sistematizada y analizada en dicha publicación con el fin de facilitar el acceso a la información."))
                                     )
                                     
                                   ),
                                   
                                   fluidRow(
                                     
                                     
                                     #### Selección de parámetros ####
                                     
                                     div(
                                       style = 'font-size: 18px; text-align: justify; background-color: #e8e6e6; padding: 10px; border-radius: 5px; margin: 5px 5px 50px; font-weight: bold;', 
                                       p(HTML("Seleccionar parámetros de búsqueda<br>")),
                                       p(HTML(" ")),
                                       p(HTML(" ")),
                                       sliderInput("yearInput",
                                                   "Período de tiempo:",
                                                   min = min(conicet$AÑO, na.rm = TRUE),
                                                   max = max(conicet$AÑO, na.rm = TRUE),
                                                   value = range(conicet$AÑO, na.rm = TRUE),
                                                   step = 1,
                                                   ticks = TRUE,
                                                   animate = FALSE,
                                                   width = "100%",
                                                   sep = ""),
                                       
                                       p(HTML("")),
                                       
                                       shinyWidgets::pickerInput("disciplinaInput", 
                                                                 "Disciplina:",
                                                                 choices = unique(conicet$Nombre_comision),
                                                                 selected = unique(conicet$Nombre_comision),
                                                                 options = list(`actions-box` = TRUE),
                                                                 multiple = TRUE,
                                                                 width = "100%"
                                       )
                                     )
                                   ),
                                   
                                   
                                   
                            )
                          )
                        ),
                        
                        
                        fluidRow(
                          
                          #Barplot de 
                          column(4, uiOutput("plot_palabras_clave")),
                          
                          column(4, leafletOutput("mapa", height = "100vh")),  
                          column(4, 
                                 fluidRow(
                                   column(12, 
                                          fluidRow(
                                            column(12,
                                                   
                                                   # <--- 1. AÑADIMOS UN SWITCH PARA AGRUPAR/DESAGRUPAR ---->
                                                   radioButtons("agrupar_grafico",
                                                                label = "Modo de visualización:",
                                                                choices = list(
                                                                  "Total" = TRUE,
                                                                  "Por Provincia" = FALSE
                                                                ),
                                                                selected = TRUE, # Inicia en "Total"
                                                                inline = TRUE # Los pone en horizontal
                                                   ),
                                                   
                                                   shinyWidgets::pickerInput(
                                                     "provincia_filter", 
                                                     "Seleccione una/s provincia/s para ver la cantidad de proyectos según el período seleccionado",
                                                     choices = lista_provincias_agrupada, 
                                                     selected = unique(conicet$PROVINCIA), 
                                                     
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       `select-all-text` = "Seleccionar Todo",
                                                       `deselect-all-text` = "Deseleccionar Todo"
                                                     ), 
                                                     
                                                     multiple = TRUE,
                                                     width = "100%"
                                                   )
                                            ),
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
                        style = "width:100%; padding:100px; background-color:#f0f5f9;"
                      )
                      
                      
                      
             ),
             
             
             
             #### Pestaña de tabla con datos ####
             
             
             tabPanel("Datos",
                      
                      tags$style(
                        HTML("
                              @import url('https://fonts.googleapis.com/css2?family=Montserrat:ital,wght@0,100..900;1,100..900&display=swap');
                              
                              .mi-titulo-h2 {
                              font-family: 'Montserrat', sans-serif;
                              font-weight: bold;
                              font-size: 22px;
                              }
                              
                             .reactable {
                             font-family: 'Montserrat', sans-serif;
                             font-weight: 550;
                             font-size: 10px; 
                             }"
                             )
                      ),
                      
                      fluidRow(
                        
                        column(10, 
                               HTML("<h2 class='mi-titulo-h2'>Visualización de la tabla de datos</h2>"),
                               
                               HTML("En esta pestaña, podrás explorar la tabla de datos utilizada en el dashboard.<br>
                               
                                    En el buscador de abajo podrás explorar palabras en toda la tabla,
                                    o utilizar el buscador incluido en cada columna.
                                    Además, cada columna puede ser ordenada en forma ascendente o descendente.")
                               
                        ),
                        
                        # column(8, 
                        #        
                        #        div(style = "height: 100px;"),
                        #        
                        #        actionButton("reset_sort", "Resetear tabla"),
                        #        
                        # ),
                        
                        
                      ),
                      
                      
                      
                      

                      hr(),
                      
                      
                      
                      tags$style(HTML(".reactable { font-size: 10px; }")),
                      
                      matchSorterDep,
                      
                      reactableOutput("data")
                      
                      

             )
  )

