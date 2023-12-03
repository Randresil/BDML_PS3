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
       tidymodels,
       caret)    

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


## Trabajo de chequeo variables en test y train. Vars interes ---------
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
# Cuartos -        P5000 - Cuartos en el hogar
# Cuartos_dormir - P5010 - Cuartos para dormir
# EstadoVivienda - P5090 - (Factor) Vivienda y estado
# Amort_mes      - P5100 - Pago mensual por cuota amort
# Est_amort_mes  - P5130 - Estimacion de pago mensual
# Arriendo_mes   - P5140 - Pago mensual de arriendo
# Personas_hogar - Nper  - Numero personas en hogar
# Personas_gasto - Npersug - Numero personas en unidad de gasto
# Li    - Linea de indigencia
# Lp    - Linea de Pobreza
# Depto - (Factor) Departamento

# Pobre - DUMMY en TRAIN
# Ingpcug - “Ingreso percápita de la unidad de gasto con imputación de arriendo a propietarios y usufructuarios”
skim(train_hogares)

# Chequeo de los duplicados
sum(duplicated(test_hogares$id))
sum(duplicated(train_hogares$id))

# rename(data, new = old)
test_hogares <- test_hogares %>% rename(Cuartos = P5000, 
                                        Cuartos_dormir = P5010, 
                                        EstadoVivienda = P5090,
                                        Amort_mes = P5100,
                                        Est_amort_mes = P5130,
                                        Arriendo_mes  = P5140,
                                        Personas_hogar = Nper,
                                        Personas_gasto = Npersug)

train_hogares <- train_hogares %>% rename(Cuartos = P5000, 
                                          Cuartos_dormir = P5010, 
                                          EstadoVivienda = P5090,
                                          Amort_mes = P5100,
                                          Est_amort_mes = P5130,
                                          Arriendo_mes  = P5140,
                                          Personas_hogar = Nper,
                                          Personas_gasto = Npersug)

test_hogares <- test_hogares %>% mutate(EstadoVivienda = case_when(EstadoVivienda == 1 ~ "Propia",
                                                                   EstadoVivienda == 2 ~ "Propia-Pagando",
                                                                   EstadoVivienda == 3 ~ "Arriendo",
                                                                   EstadoVivienda == 4 ~ "Usufructo",
                                                                   EstadoVivienda == 5 ~ "Posesion_sin_titulo",
                                                                   EstadoVivienda == 6 ~ "Otra"))
  
train_hogares <- train_hogares %>% mutate(EstadoVivienda = case_when(EstadoVivienda == 1 ~ "Propia",
                                                                     EstadoVivienda == 2 ~ "Propia-Pagando",
                                                                     EstadoVivienda == 3 ~ "Arriendo",
                                                                     EstadoVivienda == 4 ~ "Usufructo",
                                                                     EstadoVivienda == 5 ~ "Posesion_sin_titulo",
                                                                     EstadoVivienda == 6 ~ "Otra"))

# Definimos las variables categóricas para hogares
variables_categoricas_hogares <- c("EstadoVivienda",
                                   "Depto")

test_hogares <- test_hogares %>% mutate_at(variables_categoricas_hogares, as.factor)
train_hogares <- train_hogares %>% mutate_at(variables_categoricas_hogares, as.factor)




# Variables de PERSONAS TEST
skim(test_personas)
# La columna id identifica el hogar y orden es la identif<icación de persona
# id    - identificador
# Orden - Llave de persona
# P6020 - Dummy, Sexo (1 hombre, 2 mujer)
# P6040 - Años cumplidos
# P6050 - (Factor) Parentesco con jefe
# P6090 - Afiliado/cotizante/beneficiario seg salud
# P6100 - (Factor) Regimenes de seg social salud
# P6210 - (Factor) Nivel educ mas alto (a, b, c, d, e, f, g)
# P6240 - (Factor) Actividad ocupada semana pasada
# P6426 - Tiempo en trabajo
# P6430 - (Factor) Tipo posicion en ocupacion primera actividad
# P6800 - Horas a la semana trabajadas normalmente
# P6920 - (Factor) Cotizando en fondo de pensiones (1 si, 2 no, 3 pensionado)
# P7040 - Dummy, Tenia otro trabajo o negocio en semana pasada ademas de oc principal? (1 si, 2 no)
# P7090 - Dummy, Ademas de horas trabajadas, quiere trabajar mas? (1 si, 2 no)
# P7495 - Dummy, Pagos de arriendos o pensiones (1 si, 2 no)
# P7505 - Dummy, recibir dinero varias fuentes (1 si, 2 no)
# Pet   - Dummy, Poblacion en edad de trabajar (1 si, 0 no)
# Oc    - Dummy, Ocupado (1 si)
# Des   - Dummy, Desocupado (1 si)
# Ina   - Dummy, Inactivo (1 si)
# Depto - (Factor) Departamento

# Ingtot - TRAIN Ingreso total
skim(train_personas)


# Definimos las variables categóricas para personas
variables_categoricas_personas <- c("P6020",
                                   "P6050",
                                   "P6100",
                                   "P6210",
                                   "P6240",
                                   "P6430",
                                   "P6920",
                                   "Depto")

train_personas  <- train_personas %>% mutate_at(variables_categoricas_personas, as.factor)
test_personas <- test_personas %>% mutate_at(variables_categoricas_personas, as.factor)


