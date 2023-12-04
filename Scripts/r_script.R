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



## Variables de HOGARES TEST -------------
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




## Variables de PERSONAS TEST ---------
skim(test_personas)
# La columna id identifica el hogar y orden es la identif<icación de persona
# id    - identificador
# Orden - Llave de persona
# Sexo  -       P6020 - Dummy, Sexo (1 hombre, 2 mujer)
# Edad  -       P6040 - Años cumplidos
# Parentesco -  P6050 - (Factor) Parentesco con jefe
# Estado_SegS - P6090 - Afiliado/cotizante/beneficiario seg salud
# Reg_Salud -   P6100 - (Factor) Regimenes de seg social salud
# NivelEduc -   P6210 - (Factor) Nivel educ mas alto (a, b, c, d, e, f, g)
# Actividad -   P6240 - (Factor) Actividad ocupada semana pasada
# Tiempo_Trabajo - P6426 - Tiempo en trabajo
# Posicion_Ocu -   P6430 - (Factor) Tipo posicion en ocupacion primera actividad
# Horas_trabajo -  P6800 - Horas a la semana trabajadas normalmente
# Pension -      P6920 - (Factor) Cotizando en fondo de pensiones (1 si, 2 no, 3 pensionado)
# Otro_trabajo - P7040 - Dummy, Tenia otro trabajo o negocio en semana pasada ademas de oc principal? (1 si, 2 no)
# Mas_horas_Trabajo - P7090 - Dummy, Ademas de horas trabajadas, quiere trabajar mas? (1 si, 2 no)
# Pago_arr_pen - P7495 - Dummy, Pagos de arriendos o pensiones (1 si, 2 no)
# Pago_otros -   P7505 - Dummy, recibir dinero varias fuentes (1 si, 2 no)
# Pet   - Dummy, Poblacion en edad de trabajar (1 si, 0 no)
# Oc    - Dummy, Ocupado (1 si)
# Des   - Dummy, Desocupado (1 si)
# Ina   - Dummy, Inactivo (1 si)
# Depto - (Factor) Departamento

# Ingtot - TRAIN Ingreso total
skim(train_personas)

# rename(data, new = old)
test_personas <- test_personas %>% rename(Sexo = P6020,
                                          Edad = P6040,
                                          Parentesco = P6050,
                                          Estado_SegS = P6090,
                                          Reg_Salud = P6100,
                                          NivelEduc = P6210,
                                          Actividad = P6240,
                                          Tiempo_Trabajo = P6426,
                                          Posicion_Ocu = P6430,
                                          Horas_trabajo = P6800,
                                          Pension = P6920,
                                          Otro_trabajo = P7040,
                                          Mas_horas_Trabajo = P7090,
                                          Pago_arr_pen = P7495,
                                          Pago_otros = P7505,
                                          Educacion = P6210s1)

# rename(data, new = old)
train_personas <- train_personas %>% rename(Sexo = P6020,
                                            Edad = P6040,
                                            Parentesco = P6050,
                                            Estado_SegS = P6090,
                                            NivelEduc = P6210,
                                            Reg_Salud = P6100,
                                            Actividad = P6240,
                                            Tiempo_Trabajo = P6426,
                                            Posicion_Ocu = P6430,
                                            Horas_trabajo = P6800,
                                            Pension = P6920,
                                            Otro_trabajo = P7040,
                                            Mas_horas_Trabajo = P7090,
                                            Pago_arr_pen = P7495,
                                            Pago_otros = P7505,
                                            Educacion = P6210s1)

unique(test_personas$Parentesco)
unique(test_personas$Reg_Salud)
unique(test_personas$NivelEduc)
unique(test_personas$Actividad)
unique(test_personas$Posicion_Ocu)
unique(test_personas$Pension)
unique(test_personas$Otro_trabajo)
unique(test_personas$Mas_horas_Trabajo)
unique(test_personas$Pago_arr_pen)
unique(test_personas$Pago_otros)

test_personas <- test_personas %>% mutate(EstadoVivienda = case_when(Sexo == 1 ~ "hombre",
                                                                     Sexo == 2 ~ "mujer"),
                                          Parentesco = case_when(Parentesco == 1 ~ "Jefe",
                                                                 Parentesco == 2 ~ "Pareja",
                                                                 Parentesco == 3 ~ "Hijo",
                                                                 Parentesco == 4 ~ "Hijastro",
                                                                 Parentesco == 5 ~ "Nieto",
                                                                 Parentesco == 6 ~ "Otro pariente",
                                                                 Parentesco == 7 ~ "Empleado",
                                                                 Parentesco == 8 ~ "Pensionista",
                                                                 Parentesco == 9 ~ "Otro no pariente"),
                                          
                                          Estado_SegS = case_when(Estado_SegS == 1 ~ "si",
                                                                  Estado_SegS == 2 ~ "no",
                                                                  Estado_SegS == 9 ~ "No sabe"),
                                          
                                          Reg_Salud = case_when(Reg_Salud == 1 ~ "Contributivo",
                                                               Reg_Salud == 2 ~ "Especial",
                                                               Reg_Salud == 3 ~ "Subsidiado",
                                                               Reg_Salud == 9 ~ "No sabe"),
                                          
                                          NivelEduc = case_when(NivelEduc == 1 ~ "Ninguno",
                                                                NivelEduc == 2 ~ "Preescolar",
                                                                NivelEduc == 3 ~ "BasicaPrimaria",
                                                                NivelEduc == 4 ~ "BasicaSecundaria",
                                                                NivelEduc == 5 ~ "Media",
                                                                NivelEduc == 6 ~ "Superior",
                                                                NivelEduc == 9 ~ "No sabe"),
                                          
                                          Actividad = case_when(NivelEduc == 1 ~ "Trabajando",
                                                                NivelEduc == 2 ~ "Buscando",
                                                                NivelEduc == 3 ~ "Estudiando",
                                                                NivelEduc == 4 ~ "Oficios",
                                                                NivelEduc == 5 ~ "Incapacitado",
                                                                NivelEduc == 6 ~ "Otro"),
                                          
                                          Posicion_Ocu = case_when(Posicion_Ocu == 1 ~ "Obrero Particular",
                                                                   Posicion_Ocu == 2 ~ "Obrero Gobierno",
                                                                   Posicion_Ocu == 3 ~ "Domestico",
                                                                   Posicion_Ocu == 4 ~ "Propio",
                                                                   Posicion_Ocu == 5 ~ "Empleador",
                                                                   Posicion_Ocu == 6 ~ "Familiar",
                                                                   Posicion_Ocu == 9 ~ "Otro"),
                                          
                                          Pension = case_when(Pension == 1 ~ "Si",
                                                              Pension == 2 ~ "No",
                                                              Pension == 3 ~ "Pensionado"),
                                          
                                          Otro_trabajo = case_when(Otro_trabajo == 1 ~ "Si",
                                                                   Otro_trabajo == 2 ~ "No"),
                                          
                                          Mas_horas_Trabajo = case_when(Mas_horas_Trabajo == 1 ~ "Si",
                                                                        Mas_horas_Trabajo == 2 ~ "No"),
                                          
                                          Pago_arr_pen = case_when(Pago_arr_pen == 1 ~ "Si",
                                                                   Pago_arr_pen == 2 ~ "No"),
                                          
                                          Pago_otros = case_when(Pago_otros == 1 ~ "Si",
                                                                 Pago_otros == 2 ~ "No")
                                          )
                                                                 


train_personas <- train_personas %>% mutate(EstadoVivienda = case_when(Sexo == 1 ~ "hombre",
                                                                     Sexo == 2 ~ "mujer"),
                                          Parentesco = case_when(Parentesco == 1 ~ "Jefe",
                                                                 Parentesco == 2 ~ "Pareja",
                                                                 Parentesco == 3 ~ "Hijo",
                                                                 Parentesco == 4 ~ "Hijastro",
                                                                 Parentesco == 5 ~ "Nieto",
                                                                 Parentesco == 6 ~ "Otro pariente",
                                                                 Parentesco == 7 ~ "Empleado",
                                                                 Parentesco == 8 ~ "Pensionista",
                                                                 Parentesco == 9 ~ "Otro no pariente"),
                                          
                                          Estado_SegS = case_when(Estado_SegS == 1 ~ "si",
                                                                  Estado_SegS == 2 ~ "no",
                                                                  Estado_SegS == 9 ~ "No sabe"),
                                          
                                          Reg_Salud = case_when(Reg_Salud == 1 ~ "Contributivo",
                                                                Reg_Salud == 2 ~ "Especial",
                                                                Reg_Salud == 3 ~ "Subsidiado",
                                                                Reg_Salud == 9 ~ "No sabe"),
                                          
                                          NivelEduc = case_when(NivelEduc == 1 ~ "Ninguno",
                                                                NivelEduc == 2 ~ "Preescolar",
                                                                NivelEduc == 3 ~ "BasicaPrimaria",
                                                                NivelEduc == 4 ~ "BasicaSecundaria",
                                                                NivelEduc == 5 ~ "Media",
                                                                NivelEduc == 6 ~ "Superior",
                                                                NivelEduc == 9 ~ "No sabe"),
                                          
                                          Actividad = case_when(NivelEduc == 1 ~ "Trabajando",
                                                                NivelEduc == 2 ~ "Buscando",
                                                                NivelEduc == 3 ~ "Estudiando",
                                                                NivelEduc == 4 ~ "Oficios",
                                                                NivelEduc == 5 ~ "Incapacitado",
                                                                NivelEduc == 6 ~ "Otro"),
                                          
                                          Posicion_Ocu = case_when(Posicion_Ocu == 1 ~ "Obrero Particular",
                                                                   Posicion_Ocu == 2 ~ "Obrero Gobierno",
                                                                   Posicion_Ocu == 3 ~ "Domestico",
                                                                   Posicion_Ocu == 4 ~ "Propio",
                                                                   Posicion_Ocu == 5 ~ "Empleador",
                                                                   Posicion_Ocu == 6 ~ "Familiar",
                                                                   Posicion_Ocu == 9 ~ "Otro"),
                                          
                                          Pension = case_when(Pension == 1 ~ "Si",
                                                              Pension == 2 ~ "No",
                                                              Pension == 3 ~ "Pensionado"),
                                          
                                          Otro_trabajo = case_when(Otro_trabajo == 1 ~ "Si",
                                                                   Otro_trabajo == 2 ~ "No"),
                                          
                                          Mas_horas_Trabajo = case_when(Mas_horas_Trabajo == 1 ~ "Si",
                                                                        Mas_horas_Trabajo == 2 ~ "No"),
                                          
                                          Pago_arr_pen = case_when(Pago_arr_pen == 1 ~ "Si",
                                                                   Pago_arr_pen == 2 ~ "No"),
                                          
                                          Pago_otros = case_when(Pago_otros == 1 ~ "Si",
                                                                 Pago_otros == 2 ~ "No"))    
                                                                     

# Definimos las variables categóricas para personas
variables_categoricas_personas <- c("Sexo",
                                   "Parentesco",
                                   "Estado_SegS",
                                   "NivelEduc",
                                   "Reg_Salud",
                                   "Actividad",
                                   "Posicion_Ocu",
                                   "Pension",
                                   "Otro_trabajo",
                                   "Mas_horas_Trabajo",
                                   "Pago_arr_pen",
                                   "Pago_otros",
                                   "Depto")

train_personas  <- train_personas %>% mutate_at(variables_categoricas_personas, as.factor)
test_personas <- test_personas %>% mutate_at(variables_categoricas_personas, as.factor)



## Creacion variables desde Personas a Hogares -----------
# Creacion de variables de edad de jefe de hogar

edad_jefe <- train_personas %>% filter(Parentesco == "Jefe") %>% 
                group_by(id) %>% 
                  summarise(edad_max = max(Edad))
summary(edad_jefe)
train_hogares <- left_join(train_hogares,edad_jefe)

edad_promedio <- train_personas %>% 
  group_by(id) %>% 
  summarise(Promedio_edad = mean(Edad, na.rm = TRUE))
summary(edad_promedio)
train_hogares <- left_join(train_hogares,edad_promedio)



edad_jefe <- test_personas %>% filter(Parentesco == "Jefe") %>% 
              group_by(id) %>% 
                summarise(edad_max = max(Edad))
summary(edad_jefe)
test_hogares <- left_join(test_hogares,edad_jefe)

edad_promedio <- test_personas %>% 
  group_by(id) %>% 
  summarise(Promedio_edad = mean(Edad, na.rm = TRUE))
summary(edad_promedio)
test_hogares <- left_join(test_hogares,edad_promedio)



