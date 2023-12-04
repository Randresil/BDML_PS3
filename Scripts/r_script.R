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

test_personas <- test_personas %>% mutate(Sexo = case_when(Sexo == 1 ~ "hombre",
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
                                          
                                          Actividad = case_when(Actividad == 1 ~ "Trabajando",
                                                                Actividad == 2 ~ "Buscando",
                                                                Actividad == 3 ~ "Estudiando",
                                                                Actividad == 4 ~ "Oficios",
                                                                Actividad == 5 ~ "Incapacitado",
                                                                Actividad == 6 ~ "Otro"),
                                          
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
                                                                 


train_personas <- train_personas %>% mutate(Sexo = case_when(Sexo == 1 ~ "hombre",
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
                                          
                                          Actividad = case_when(Actividad == 1 ~ "Trabajando",
                                                                Actividad == 2 ~ "Buscando",
                                                                Actividad == 3 ~ "Estudiando",
                                                                Actividad == 4 ~ "Oficios",
                                                                Actividad == 5 ~ "Incapacitado",
                                                                Actividad == 6 ~ "Otro"),
                                          
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
# Test hogares con 66168 obs
# Train hogares con 164960 obs
write.csv(train_personas, file = "train_personas2.csv")
write.csv(test_personas, file = "test_personas2.csv")
write.csv(train_hogares, file = "train_hogares2.csv")
write.csv(test_hogares, file = "test_hogares2.csv")

rm(list = ls())

train_personas <- read.csv("train_personas2.csv")
train_hogares <- read.csv("train_hogares2.csv")
test_personas <- read.csv("test_personas2.csv")
test_hogares <- read.csv("test_hogares2.csv")


colnames(train_personas)
colnames(test_personas)


edad_jefe <- train_personas %>% filter(Parentesco == "Jefe") %>% 
                group_by(id) %>% 
                  summarise(edad_max = max(Edad))
summary(edad_jefe)
train_hogares <- left_join(train_hogares,edad_jefe, by = "id")
rm(edad_jefe)


edad_promedio <- train_personas %>% 
  group_by(id) %>% 
  summarise(Promedio_edad = mean(Edad, na.rm = TRUE))
summary(edad_promedio)
train_hogares <- left_join(train_hogares,edad_promedio, by = "id")
rm(edad_promedio)


educacion_suma <- train_personas %>% 
  group_by(id) %>% 
  summarise(tiempo_edu = sum(Educacion, na.rm = TRUE))
summary(educacion_suma)
train_hogares <- left_join(train_hogares,educacion_suma, by = "id")
rm(educacion_suma)


educacion_superior <- train_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(educ_superior = ifelse(NivelEduc == "Superior", 1, 0))
summary(educacion_superior)
train_hogares <- left_join(train_hogares, educacion_superior, by = "id")
rm(educacion_superior)


educacion_media <- train_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(educ_media = ifelse(NivelEduc == "Media", 1, 0))
summary(educacion_media)
train_hogares <- left_join(train_hogares, educacion_media, by = "id")
rm(educacion_media)


reg_salud <- train_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(regimen_salud_cont = ifelse(Reg_Salud == "Contributivo", 1, 0))
summary(reg_salud)
train_hogares <- left_join(train_hogares,reg_salud, by = "id")
rm(reg_salud)


trabajo_suma <- train_personas %>% 
  group_by(id) %>% 
  summarise(horas_trabajo_total = sum(Horas_trabajo, na.rm = TRUE))
summary(trabajo_suma)
train_hogares <- left_join(train_hogares,trabajo_suma, by = "id")
rm(trabajo_suma)


cot_pension <- train_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(pension = ifelse(Pension == "Si", 1, 0))
summary(cot_pension)
train_hogares <- left_join(train_hogares, cot_pension, by = "id")
rm(cot_pension)


pago_arriendo_pen <- train_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(Pago_arr_pen = ifelse(Pago_arr_pen == "Si", 1, 0))
summary(pago_arriendo_pen)
train_hogares <- left_join(train_hogares, pago_arriendo_pen, by = "id")
rm(pago_arriendo_pen)


pago_otros <- train_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(Pago_otros = ifelse(Pago_otros == "Si", 1, 0))
summary(pago_otros)
train_hogares <- left_join(train_hogares, pago_otros, by = "id")
rm(pago_otros)


desempleo <- train_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(desempleo = ifelse(Actividad == "Buscando", 1, 0))
summary(desempleo)
train_hogares <- left_join(train_hogares, desempleo, by = "id")
rm(desempleo)


train_hogares <- train_hogares %>% mutate(hogar_propio_arr = ifelse(EstadoVivienda == "Propia" | EstadoVivienda == "Propia-Pagando" | EstadoVivienda == "Arriendo", 1, 0))
train_hogares <- train_hogares %>% mutate(hogar_usu_otro = ifelse(EstadoVivienda == "Posesion_sin_titulo" | EstadoVivienda == "Otro" | EstadoVivienda == "Usufructo", 1, 0))




# Ahora con las bases de test
edad_jefe <- test_personas %>% filter(Parentesco == "Jefe") %>% 
              group_by(id) %>% 
                summarise(edad_max = max(Edad))
summary(edad_jefe)
test_hogares <- left_join(test_hogares,edad_jefe, by = "id")
rm(edad_jefe)


edad_promedio <- test_personas %>% 
  group_by(id) %>% 
  summarise(Promedio_edad = mean(Edad, na.rm = TRUE))
summary(edad_promedio)
test_hogares <- left_join(test_hogares,edad_promedio, by = "id")
rm(edad_promedio)


educacion_suma <- test_personas %>% 
  group_by(id) %>% 
  summarise(tiempo_edu = sum(Educacion, na.rm = TRUE))
summary(educacion_suma)
test_hogares <- left_join(test_hogares,educacion_suma, by = "id")
rm(educacion_suma)


educacion_superior <- test_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(educ_superior = ifelse(NivelEduc == "Superior", 1, 0))
summary(educacion_superior)
test_hogares <- left_join(test_hogares,educacion_superior, by = "id")
rm(educacion_superior)


educacion_media <- test_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(educ_media = ifelse(NivelEduc == "Media", 1, 0))
summary(educacion_media)
test_hogares <- left_join(test_hogares, educacion_media, by = "id")
rm(educacion_media)


reg_salud <- test_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(regimen_salud_cont = ifelse(Reg_Salud == "Contributivo", 1, 0))
summary(reg_salud)
test_hogares <- left_join(test_hogares,reg_salud, by = "id")
rm(reg_salud)

trabajo_suma <- test_personas %>% 
  group_by(id) %>% 
  summarise(horas_trabajo_total = sum(Horas_trabajo, na.rm = TRUE))
summary(trabajo_suma)
test_hogares <- left_join(test_hogares,trabajo_suma, by = "id")
rm(trabajo_suma)


cot_pension <- test_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(pension = ifelse(Pension == "Si", 1, 0))
summary(cot_pension)
test_hogares <- left_join(test_hogares, cot_pension, by = "id")
rm(cot_pension)


pago_arriendo_pen <- test_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(Pago_arr_pen = ifelse(Pago_arr_pen == "Si", 1, 0))
summary(pago_arriendo_pen)
test_hogares <- left_join(test_hogares, pago_arriendo_pen, by = "id")
rm(pago_arriendo_pen)


pago_otros <- test_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(Pago_otros = ifelse(Pago_otros == "Si", 1, 0))
summary(pago_otros)
test_hogares <- left_join(test_hogares, pago_otros, by = "id")
rm(pago_otros)


desempleo <- test_personas %>% filter(Parentesco == "Jefe") %>% 
  group_by(id) %>% 
  summarise(desempleo = ifelse(Actividad == "Buscando", 1, 0))
summary(desempleo)
test_hogares <- left_join(test_hogares, desempleo, by = "id")
rm(desempleo)


test_hogares <- test_hogares %>% mutate(hogar_propio_arr = ifelse(EstadoVivienda == "Propia" | EstadoVivienda == "Propia-Pagando" | EstadoVivienda == "Arriendo", 1, 0))
test_hogares <- test_hogares %>% mutate(hogar_usu_otro = ifelse(EstadoVivienda == "Posesion_sin_titulo" | EstadoVivienda == "Otro" | EstadoVivienda == "Usufructo", 1, 0))





## MODELOS ------------------
# Modelo de RF
# Variables a considerar:
# Cuartos + Cuartos_dormir + EstadoVivienda + Est_amort_mes + Personas_hogar + Depto + edad_max + tiempo_edu + educ_superior + educ_media + regimen_salud_cont + horas_trabajo_total + pension + Pago_arr_pen + Pago_otros + desempleo 
train_hogares$Pobre <- as.factor(train_hogares$Pobre)

test_hogares <- test_hogares %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_if(is.factor, as.numeric) %>%
  mutate_if(is.logical, as.numeric)



rf_grid_random <- grid_random(mtry(range = c(2, 8)),
                              min_n(range = c(1, 10)),
                              trees(range = c(100, 200)), size = 4)

# EstadoVivienda me arroja errores
receta1 <- recipe(Pobre ~ Cuartos + Cuartos_dormir + Est_amort_mes + Personas_hogar + Depto + edad_max + tiempo_edu + educ_superior + educ_media + regimen_salud_cont +
                    horas_trabajo_total + pension + Pago_arr_pen + Pago_otros + desempleo, data = train_hogares) %>%
  step_impute_mean(all_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

rf_spec <- rand_forest(
  trees = tune(),
  min_n =   tune(),
  mtry =  tune()) %>% 
  set_engine("randomForest") %>%
  set_mode("classification")

workflow1 <- workflow() %>%
  add_recipe(receta1) %>%
  add_model(rf_spec)

df_fold <- vfold_cv(train_hogares, v = 4)

tune_rf <- tune_grid(
  workflow1,
  resamples = df_fold, 
  grid = rf_grid_random,
  metrics = metric_set(f_meas))


# Obtener el mejor hiper-parámetro
best_params_rf <- tune_rf %>% select_best(metric = "f_meas")
best_params_rf

# Entrenar el modelo con el mejor hiper-parámetro
best_rf_fit <- finalize_workflow(workflow1, best_params_rf) %>%
  fit(data = train_hogares)

prediccion_random_forest1 <- predict(best_rf_fit, new_data=test_hogares)
prediccion_random_forest1$.pred_class <- as.numeric(prediccion_random_forest1$.pred_class)-1 # Para ser el 1 pobre
write.csv(test_hogares %>% select(id) %>% bind_cols(prediccion_random_forest1) %>% rename(pobre = .pred_class)
            , file = 'prediccion_forest.csv', row.names = FALSE)





receta2 <- recipe(Pobre ~ Cuartos + Cuartos_dormir + Est_amort_mes + Personas_hogar + hogar_propio_arr + Depto + edad_max + tiempo_edu + educ_superior + educ_media + regimen_salud_cont +
                    horas_trabajo_total + pension + Pago_arr_pen + Pago_otros + desempleo, data = train_hogares) %>%
  step_impute_mean(all_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

rf_spec <- rand_forest(
  trees = tune(),
  min_n =   tune(),
  mtry =  tune()) %>% 
  set_engine("randomForest") %>%
  set_mode("classification")

workflow2 <- workflow() %>%
  add_recipe(receta2) %>%
  add_model(rf_spec)

df_fold <- vfold_cv(train_hogares, v = 4)

tune_rf <- tune_grid(
  workflow2,
  resamples = df_fold, 
  grid = rf_grid_random,
  metrics = metric_set(f_meas))


# Obtener el mejor hiper-parámetro
best_params_rf <- tune_rf %>% select_best(metric = "f_meas")
best_params_rf

# Entrenar el modelo con el mejor hiper-parámetro
best_rf_fit <- finalize_workflow(workflow2, best_params_rf) %>%
  fit(data = train_hogares)

prediccion_random_forest2 <- predict(best_rf_fit, new_data=test_hogares)
prediccion_random_forest2$.pred_class <- as.numeric(prediccion_random_forest2$.pred_class)-1 # Para ser el 1 pobre
write.csv(test_hogares %>% select(id) %>% bind_cols(prediccion_random_forest2) %>% rename(pobre = .pred_class)
          , file = 'prediccion_forest2.csv', row.names = FALSE)


