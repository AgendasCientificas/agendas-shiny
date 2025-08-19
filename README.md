# Agendas científicas sobre desarrollo infantil: relevamiento y análisis de datos abiertos del CONICET

CONICET es el principal organismo de ciencia y tecnología del país. Conocer las temáticas de investigación de quienes lo integran y su localización geográfica e institucional constituye información valiosa para la exploración de agendas de investigación, el establecimiento de vínculos interinstitucionales y de redes de investigadores. Asimismo es un potencial insumo para elaboración de políticas científicas y la discusión pública basada en información accesible.

Esta información es pública, pero no es de fácil acceso.

Basándonos en las bases de datos recopiladas, analizamos el comportamiento de las becas e ingresos otorgados por organismo en el periodo 2010-2020 a candidatos y candidatas con temas vinculados al desarrollo Niños, Niñas y Adolescentes considerando asimismo su distribución geográfica y disciplinar. Estos resultados fueron publicados en el año 2024 en la revista Iberoamericana de Ciencia, Tecnología y Sociedad (Smulski, et al. 2024).

El artículo original puede leerse [aquí](https://ojs.revistacts.net/index.php/CTS/article/view/410)

Aquí presentamos un tablero interactivo que pone a disposición la información recopilada, sistematizada y analizada en dicha publicación con el fin de facilitar el acceso a la información.


## Organización del Repo
Este repositorio contiene un tablero interactivo que proporciona información valiosa sobre proyectos financiados en el área del desarrollo infantil. El tablero se ha desarrollado con el objetivo de fomentar redes de trabajo, colaboración y diálogo entre quienes investigan en este campo. La estructura del repositorio es la siguiente:

- **code/**: Esta carpeta contiene los scripts utilizados para procesar los datos y para construir la aplicación Shiny.
  - **preprocessing.R**: Código para el preprocesamiento de los datos.
  - **shiny_app.R**: Código para la creación de la aplicación Shiny.

- **data/**: Esta carpeta incluye dos conjuntos de datos.
  - **raw_data.csv**: Base de datos sin procesar.
  - **processed_data.csv**: Base de datos procesada.

## Cómo Ejecutar el Tablero
1. Clona este repositorio: 
   ```bash
   git clone https://github.com/AgendasCientificas/nombre_del_repositorio.git
