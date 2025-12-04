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
                                            column(12, plotOutput("graficoProyectosRegion", height = "50vh")), 
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
                      
                      

             ),
             
             
             
             #### About us ####
             
             tabPanel("Quienes somos",
                      
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
                            HTML("Quienes somos")
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
                            column(6, 
                                   
                                   ##### Datos Fede ####
                                   
                                   column(3,
                                     
                                          img(src = "fede_about.png",
                                              style = "width: 100%; max-width: 200px; height: auto; display: block; margin: 0 auto;"
                                              ),
                                          
                                          # div
                                          
                                      
                                          
                                          div(style = "text-align: center; margin-top: 10px; font-weight: bold; font-size:20px;",
                                              
                                              p("Fede Giovannetti"),
                                              
                                              a(href = "https://orcid.org/0000-0003-2238-3674", # Reemplaza con la URL de tu página
                                                target = "_blank", # Abre en una nueva pestaña
                                                icon("orcid", class = "fas fa-1x", style = "margin-right:10%; font-size:28px;") 
                                              ),
                                              
                                              a(href = "https://fedegiovannetti.github.io/es", # Reemplaza con la URL de tu página
                                                target = "_blank", # Abre en una nueva pestaña
                                                icon("globe", class = "fas fa-1x", style = "margin-right:10%; font-size:28px;") 
                                              ),
                                              
                                              a(href = "mailto:giovannettipsi@gmail.com", # Reemplaza con tu dirección de correo
                                                target = "_blank", # Abre en una nueva pestaña
                                                icon("envelope", class = "fas fa-1x", style = "font-size:28px;") 
                                              )
                                            )
                                          ),
                                   
                                   column(9,
                                            markdown("Soy Argentino, nacido en el partido de Quilmes. Me recibí como Doctor en Psicología
                                                  en la Universidad Nacional de Córdoba (UNC) y como Licenciado en Psicología en la Universidad Favaloro.
                                                  En el ámbito de la investigación, me desempeño como Becario postdoctoral CONICET en la 
                                                  Unidad de Neurobiología Aplicada (UNA, CEMIC-CONICET). En agosto de 2025 me salió el ingreso 
                                                  a la Carrera de Investigador Científico (CIC) para la cual estoy esperando el alta.
                                                  
                                                  Siempre me dediqué al estudio del desarrollo cognitivo infantil en contextos de vulnerabilidad social
                                                  a partir de diferentes niveles de análisis (cognitivo, neural, contextual). 
                                                  También tengo interés en el estudio de las agendas científicas sobre desarrollo infantil
                                                  en la Argentina, y el desarrollo de la psicología como disciplina científica.
                                                  
                                                  La ciencia de datos es mi principal herramienta de trabajo. Trabajo fundamentalmente con R, 
                                                  aunque cada tanto hago alguna que otra cosa con Python. Me interesa mucho poder aplicar el 
                                                  conocimiento proveniente de las ciencias de datos, la programación estadística y el machine learning a
                                                  las ciencias del desarrollo y las políticas públicas. Además, me interesa mucho colaborar en la 
                                                  construcción de una mayor transparencia, apertura y reproducibilidad en el uso de datos cognitivos.
                                                  
                                                  Ejercí y ejerzo la docencia en distintas instituciones de grado y posgrado entre las que se encuentran 
                                                  la Universidad Nacional de San Martin (UNSAM), la Universidad Nacional de Córdoba (UNC), y la Universidad Favaloro.")
                                     
                                          )

                            ),
                            
                            # Segundo recuadro
                            column(6, 
                                   
                                   ##### Datos Maru ####
                                   
                                   column(3,
                                          
                                          img(src = "maru_about.png",
                                              style = "width: 100%; max-width: 200px; height: auto; display: block; margin: 0 auto;"
                                          ),
                                          
                                          div(style = "text-align: center; margin-top: 10px; font-weight: bold; font-size:20px;",
                                              
                                              p("Mariana C. Smulski"),
                                              
                                              a(href = "https://orcid.org/0000-0003-2827-8808", # Reemplaza con la URL de tu página
                                                target = "_blank", # Abre en una nueva pestaña
                                                icon("orcid", class = "fas fa-1x", style = "margin-right:10%; font-size:28px;") 
                                              ),
                                              
                                              # a(href = "https://fedegiovannetti.github.io/es", # Reemplaza con la URL de tu página
                                              #   target = "_blank", # Abre en una nueva pestaña
                                              #   icon("globe", class = "fas fa-1x", style = "margin-right: 25px; font-size:28px;") 
                                              # ),
                                              
                                              a(href = "mailto:mcsmulski@gmail.com", # Reemplaza con tu dirección de correo
                                                target = "_blank", # Abre en una nueva pestaña
                                                icon("envelope", class = "fas fa-1x", style = "font-size:28px;") 
                                              )
                                          )
                                   ),
                                   
                                   column(9,
                                          markdown("Doctora en Ciencias Antropológicas por la Universidad de Buenos Aires (UBA). Licenciada y
                                                    profesora en Ciencias Antropológicas (UBA). Profesional Adjunta asistente de investigación
                                                    en el Centro de Investigaciones Sociales (CIS, IDES-CONICET). Docente en Epistemología
                                                    de las Ciencias Sociales (UBA).
                                                    
                                                    Me especializo en el campo de los Estudios Sociales de la Ciencia y la Tecnología. Mi 
                                                    trabajo se centra en el análisis de las dinámicas de producción, circulación y efectos del 
                                                    conocimiento neurocientífico. En particular, en los últimos años he investigado la producción 
                                                    de saberes sobre el desarrollo cognitivo infantil en contextos de pobreza, así como la forma 
                                                    en que estos conocimientos intervienen en distintos ámbitos.
                                                    
                                                    Actualmente, mi interés se orienta al estudio del modo en que las retóricas neurocientíficas
                                                    dialogan con las políticas públicas dirigidas al desarrollo integral de la primera infancia, 
                                                    especialmente en la producción de categorías que orientan las acciones gubernamentales. 
                                                    Asimismo, analizo la relación entre las agendas científicas de investigación y las agendas 
                                                    políticas locales, atendiendo a las dinámicas de la política científica. 
                                                    
                                                    A través del uso de herramientas de la ciencia de datos, busco contribuir al análisis de la producción de 
                                                    conocimientos orientados a la resolución de problemas socialmente relevantes y al 
                                                    aprovechamiento de las innovaciones científico-tecnológicas en el diseño e implementación 
                                                    de políticas públicas.")
                                          
                                   )
                                   
                            ),
                            
                            p()
                            
                          ),
                          
                          fluidRow(
                            
                            p(),
                            # style = "display: flex; flex-wrap: wrap;",
                            
                            column(6, 
                                   
                                   ##### Datos Fer ####
                                   
                                   column(3,
                                          
                                          img(src = "fer_about.png",
                                              style = "width: 100%; max-width: 200px; height: auto; display: block; margin: 0 auto;"
                                          ),
                                          
                                          div(style = "text-align: center; margin-top: 10px; font-weight: bold; font-size:20px;",
                                              
                                              p("Fernando Steeb"),
                                              
                                              a(href = "https://orcid.org/0009-0001-7707-1486", # Reemplaza con la URL de tu página
                                                target = "_blank", # Abre en una nueva pestaña
                                                icon("orcid", class = "fas fa-1x", style = "margin-right:10%; font-size:28px;") 
                                              ),
                                              
                                              a(href = "https://www.linkedin.com/in/fernando-steeb-7261ba175/", # Reemplaza con la URL de tu página
                                                target = "_blank", # Abre en una nueva pestaña
                                                icon("globe", class = "fas fa-1x", style = "margin-right:10%; font-size:28px;") 
                                              ),
                                              
                                              a(href = "mailto:fsteeb@udesa.edu.ar", # Reemplaza con tu dirección de correo
                                                target = "_blank", # Abre en una nueva pestaña
                                                icon("envelope", class = "fas fa-1x", style = "font-size:28px;") 
                                              )
                                          )
                                   ),
                                   
                                   column(9,
                                          markdown("Soy Licenciado en Psicología por la Universidad de Buenos Aires y doctorando 
                                                    en Psicología por la Universidad Nacional de Córdoba, con beca doctoral del CONICET.
                                                    Desarrollo mi proyecto en el Centro de Neurociencias Cognitivas de la Universidad de San Andrés, 
                                                    investigando cómo niñas, niños y adolescentes desarrollan preferencias de distribución de recursos
                                                    y cómo estas decisiones se ven influenciadas por la sensibilidad a principios de organización 
                                                    jerárquica y normas sociales.
                                                    
                                                    Asimismo, soy docente en la Lic. en Ciencias del Comportamiento de la Universidad de San Andrés, 
                                                    donde dicto materias vinculadas al estudio de los principios del aprendizaje y el desarrollo de los 
                                                    procesos cognitivos. También formo parte del Comité de Comunicación de la Asociación Argentina 
                                                    de Ciencias del Comportamiento, donde colaboro en acciones de divulgación científica.
                                                    
                                                    Me interesa la Ciencia de Datos y su aplicación al estudio del comportamiento humano. 
                                                    Actualmente estoy realizando el proceso de ingreso a la Maestría en Ciencia de Datos de 
                                                    la Universidad de Buenos Aires, con el objetivo de fortalecer mi formación técnica y potenciar mi práctica científica.
                                                   
                                                    A su vez, tengo un fuerte interés en la Ciencia Abierta. Busco activamente incorporar estas prácticas,
                                                    colaborar en el desarrollo de iniciativas que promuevan y faciliten su adopción, y contribuir a
                                                    proyectos de big team science orientados a mejorar la generalizabilidad y robustez de la investigación en 
                                                    ciencias del comportamiento.")
                                          
                                   )
                                   
                            ),
                            
                            # Segundo recuadro
                            column(6, 
                                   
                                   ##### Datos Flor ####
                                   
                                   column(3,
                                          
                                          img(src = "flor_about.png",
                                              style = "width: 100%; max-width: 200px; height: auto; display: block; margin: 0 auto;"
                                          ),
                                          
                                          div(style = "text-align: center; margin-top: 10px; font-weight: bold; font-size:20px;",
                                              
                                              p("Florencia Altschuler"),
                                              
                                              a(href = "https://orcid.org/0000-0001-6362-963X", # Reemplaza con la URL de tu página
                                                target = "_blank", # Abre en una nueva pestaña
                                                icon("orcid", class = "fas fa-1x", style = "margin-right:10%; font-size:28px;") 
                                              ),
                                              
                                              a(href = "https://www.linkedin.com/in/florencia-altschuler/", # Reemplaza con la URL de tu página
                                                target = "_blank", # Abre en una nueva pestaña
                                                icon("globe", class = "fas fa-1x", style = "margin-right:10%; font-size:28px;") 
                                              ),
                                              
                                              a(href = "mailto:faltschuler@udesa.edu.ar", # Reemplaza con tu dirección de correo
                                                target = "_blank", # Abre en una nueva pestaña
                                                icon("envelope", class = "fas fa-1x", style = "font-size:28px;") 
                                              )
                                          )
                                   ),
                                   
                                   column(9,
                                          markdown("Licenciada en Ciencias Biológicas por la Universidad de Buenos Aires, 
                                                    actualmente curso la Maestría en Explotación de Datos y Descubrimiento del Conocimiento y
                                                   el Doctorado en Medicina en la misma institución. Me desempeño como Becaria Doctoral CONICET 
                                                   en el Centro de Neurociencias Cognitivas de la Universidad de San Andrés, 
                                                   donde trabajo con imágenes de resonancia magnética cerebral para estudiar 
                                                   los signos de enfermedad cerebrovascular y su asociación con la neurodegeneración y
                                                   los factores de riesgo cardiovasculares en poblaciones de América Latina. 
                                                   
                                                   Mi interés principal es integrar las ciencias de datos con la salud para abordar 
                                                   problemáticas complejas vinculadas al envejecimiento cerebral. 
                                                   
                                                   Además, soy docente en la Facultad de Ciencias Exactas y Naturales de la UBA
                                                   y en la Universidad de San Andrés.  ")
                                          
                                   )
                                   
                            )
                            
                          )
                        )
                    
                      ),
                      
                      
                      
                      
                      
                      tags$footer(
                        HTML('<span style="font-size:12px;"><i>Autores: Florencia Altschuler, Federico Giovannetti, Fernando Steeb y Mariana Smulski. </i></span><br>
        <span style="font-size:12px;">Datos obtenidos de <a href="https://ojs.revistacts.net/index.php/CTS/article/view/410" target="_blank";"><i>Smulski, M., Giovannetti, F., Steeb, F., Serra, A. L. P., Grasser, F. B., Jove, G. M., & Cevasco, J. (2024). </i></span><br> Agendas científicas sobre desarrollo infantil en CONICET: Evolución de becas e ingresos de investigadores en el periodo 2010-2020. Revista Iberoamericana de Ciencia, Tecnología y Sociedad</i></a> - CTS.</span><br>
        <span style="font-size:12px;">El código fuente de este tablero está disponible en nuestro <a href="https://github.com/AgendasCientificas" target="_blank";">repositorio de GitHub</a>.</span>'),
                        align = "left",
                        style = "width:100%; padding:5px; background-color:#f0f5f9;"
                      )
                      
                      
                      
             )
  )

