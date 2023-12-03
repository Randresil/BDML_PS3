#### RScript - BIG DATA & MACHINE LEARNING #### 
# Date: X/12/2023
# R version 4.3.2
## ------------------------------------------ ## 

# Limpiar el entorno
rm(list = ls())

## Instalación de paquetes ------------------
install.packages("pacman")
library(pacman)
p_load(tidyverse,  # Paquete grande de manipulacion
       lubridate,  # Paquete para manejo de fechas
       skimr,      # Paquete para revision de datos
       stargazer,  # Paquete de tablas "bonitas", regs y estad desc
       dplyr,      # Paquete parte de tidyverse donde esta mutate, select, filter, summarise...
       rio,        # Paquete de importacion/exportacion de datos
       keras, 
       themis,
       yardstick,
       tensorflow)    

# Manejo del directorio
getwd()
directorio <- "/Users/ricardoandressilvatorres/Documents/Universidad/Maestria/2. Segundo Semestre/Big Data - Machine Learning/Problem sets/PS3"
setwd(directorio)

# Chequeo de los archivos del directorio
dir()
list.files()


## Importacion de los datos ------------------
install_formats() # Cuestiones de importacion de archivos del paquete rio

train_personas <- read.csv("train_personas.csv")
train_hogares <- read.csv("train_hogares.csv")

test_personas <- read.csv("test_personas.csv")
test_hogares <- read.csv("test_hogares.csv")


## Aqui miramos que variables hay y el orden. ID es de hogar y orden es de persona
colnames(train_personas)
colnames(train_hogares)
