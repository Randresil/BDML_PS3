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
       discrim,
       themis,
       yardstick,
       tensorflow,
       tidymodels)    

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


# Trabajo con todas las bases en hogares y train
train_hogares <- train_hogares %>% mutate(train = 1)
test_hogares <- test_hogares %>% mutate(train = 0)

train_personas <- train_personas %>% mutate(train = 1)
test_personas <- test_personas %>% mutate(train = 0)


## Aqui miramos que variables hay y el orden. ID es de hogar y orden es de persona
colnames(train_personas) == colnames(test_personas)
colnames(train_hogares) == colnames(test_hogares)


"Lp" %in% colnames(train_personas)
"Lp" %in% colnames(train_hogares)
object.size(train_personas)

colnames(test_personas)
colnames(train_personas)

colnames(test_hogares)
colnames(train_hogares)

# Variables de HOGARES TEST
skim(test_hogares)
# P5000 - Cuartos en el hogar
# P5010 - Cuartos para dormir
# P5090 - (Factor) Vivienda y estado
# P5100 - Pago pensual por cuota amort
# P5130 - Estimacion de pago mensual
# P5140 - Pago mensual de arriendo
# Nper  - Numero personas en hogar
# Npersug - Numero personas en unidad de gasto
# Li    - Linea de indigencia
# Lp    - Linea de Pobreza
# Depto - (Factor) Departamento

# Pobre - DUMMY en TRAIN


# Variables de PERSONAS TEST
skim(test_personas)
# La columna id identifica el hogar y orden es la identificación de persona
# id    - identificador
# Orden - Llave de persona
# P6020 - Sexo (1 hombre, 2 mujer)
# P6040 - Años cumplidos
# P6050 - Parentesco con jefe
# P6090 - Afiliado/cotizante/beneficiario seg salud
# P6100 - (Factor) Regimenes de seg social salud
# 

