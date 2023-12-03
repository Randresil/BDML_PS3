rm(list =ls ())


train_personas <- read.csv("~/Octavo semestre/Big data and machine learning/Taller 3/train_personas.csv")
train_hogares <- read.csv("~/Octavo semestre/Big data and machine learning/Taller 3/train_hogares.csv")

test_personas <- read.csv("Octavo semestre/Big data and machine learning/Taller 3/test_personas.csv")
test_hogares <- read.csv("Octavo semestre/Big data and machine learning/Taller 3/test_hogares.csv")

library(pacman) 
library(dplyr)
require("pacman")
p_load("tidyverse")

## Aqui miramos que variables hay y el orden. ID es de hogar y orden es de persona
colnames(train_personas)[1:3]
colnames(train_hogares)

colnames(train_personas)

## CREAR VARIABLES CON PERSONAS PARA TRAIN


caracteristicas_edad <- train_personas %>%
  group_by(id) %>%
  summarize(
    cantidad_personas = n(),
    suma_edades = sum(P6040),
    edad_prom_hogar = round(suma_edades / cantidad_personas, 1),
  )

# Creamos una nueva variable dummy llamada Iof2_dummy
train_personas$Iof2_dummy <- ifelse(train_personas$Iof2 > 1, 1, ifelse(is.na(train_personas$Iof2) | train_personas$Iof2 <= 1, 0, NA))

train_personas %>%
  count(Iof2_dummy) %>% head() 

## esta es la dummy de personas pensionadas, toca meterla a la variable de hogar.

sum(is.na(train_personas$Iof1))
train_personas %>%
  count(Iof1) %>% head() 


# Creamos una nueva variable dummy llamada Iof1_dummy
train_personas$Iof1_dummy <- ifelse(train_personas$Iof1 > 1, 1, ifelse(is.na(train_personas$Iof1) | train_personas$Iof1 <= 1, 0, NA))

train_personas %>%
  count(Iof1_dummy) %>% head() 
## Dummy de si recibe dividendos


##______ AQUI LOS LLAMO COMO DEBERIAN

train_personas$dummy_pension <- train_personas$Iof2_dummy
train_personas$dummy_dividendos <- train_personas$Iof1_dummy


train_personas %>%
  count(dummy_dividendos) %>% head() 
## aqui cree una variable  que pegare a la otra (pero que hacer con los NA)



dummy_pension_hogar <- train_personas %>%
  group_by(id) %>%
  summarize(pension_hogar = max(dummy_pension))


dummy_dividendis_hogar <- train_personas %>%
  group_by(id) %>%
  summarize(dividendos_hogar = max(dummy_dividendos))


## PEGAR LAS DUMMYS A LA BASE DE HOGAR

train_hogares<-left_join(train_hogares, dummy_pension_hogar)
colnames(train_hogares)


train_hogares<-left_join(train_hogares, dummy_dividendis_hogar)
colnames(train_hogares)

train_hogares<-left_join(train_hogares,caracteristicas_edad)
colnames(train_hogares)


## HASTA ACA YA HAY 3 (DIVIDENDOS NO ESTA EN TEST)

## imputar con la mediana 
mediana_arriendo <- median(train_hogares$P5130, na.rm = TRUE) ## p5130 es el pago de arrendamiento

train_hogares <- train_hogares %>%
  mutate(P5130 = replace_na(P5130,mediana_arriendo ))


## la de casa propia
train_hogares$casa_propia_paga <- ifelse(train_hogares$P5090 == 1, 1, 0)


## ESTRATO NO ESTA EN TEST
dummy_estrato <- train_personas %>%
  group_by(id) %>%
  summarize(estrato_hogar = max(Estrato1))



train_hogares<-left_join(train_hogares,dummy_estrato)
colnames(train_hogares)


train_personas %>%
  summarise_all(~sum(is.na(.))) %>% transpose()


## personas en edad de trabajar

personas_Pet <- train_personas %>%
  group_by(id) %>%
  summarize(cantidad_pet_hogar = sum(Pet, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cantidad_pet_hogar = replace(cantidad_pet_hogar, is.na(cantidad_pet_hogar), 0))

train_hogares<-left_join(train_hogares,personas_Pet)
colnames(train_hogares)




train_hogares <- train_hogares %>%
  mutate(ingreso_miembro = Ingtotug / cantidad_personas)


# Mediana de educación
mediana_pension <- median(train_personas$P6210s1, na.rm = TRUE) # P6210s1 es educación

# Reemplazar los valores NA con la mediana
train_personas <- train_personas %>%
  mutate(P7495 = replace_na(P6210s1, mediana_pension))

# Calcular el promedio de educación por hogar
educ_promedio <- train_personas %>%
  group_by(id) %>%
  summarize(
    cantidad_personas1 = n(),
    suma_educacion = sum(P6210s1),
    educ_prom_hogar = round(suma_educacion / cantidad_personas1, 1)
  )


# Pegar el promedio de educación a la base de hogares
train_hogares <- left_join(train_hogares, educ_promedio)
colnames(train_hogares)

summary(train_hogares$educ_prom_hogar)

mean_educ <- mean(train_hogares$educ_prom_hogar, na.rm = TRUE)

train_hogares <- train_hogares %>%
  mutate(educ_prom_hogar = replace_na(educ_prom_hogar, mean_educ))



# Crear la variable dummy con 0 para missing values
train_personas$desempleados <- ifelse(train_personas$P6240 == 2, 1, ifelse(is.na(train_personas$P6240), 0, 0))
summary(train_personas$desempleados)

# Resumir y agregar a train_hogares
desempleados <- train_personas %>%
  group_by(id) %>%
  summarize(desempleados = max(desempleados))

# Unir con train_hogares
train_hogares <- left_join(train_hogares, desempleados, by = "id")
colnames(train_hogares)

library(dplyr)

mediana_desempleado <- median(train_hogares$desempleados, na.rm = TRUE)

train_hogares <- train_hogares %>%
  mutate(desempleados= replace_na(desempleados, mediana_desempleado))

mediana_pension <- median(train_hogares$pension_hogar, na.rm = TRUE)

train_hogares <- train_hogares %>%
  mutate(pension_hogar= replace_na(pension_hogar, mediana_pension))




##__________________________-AHORA LO MISMO EN TEST---------------------------



# Ahora puedes utilizar los nuevos nombres (test_hogares y test_personas) en tu código

test_personas %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

summary(test_personas$P6100)
hist(test_personas$P6100)

hist(test_personas$P6040)
summary(test_personas$P6040)
str(test_personas$P6040)
test_personas %>%
  count(P6040)

caracteristicas_edad <- test_personas %>%
  group_by(id) %>%
  summarize(
    cantidad_personas = n(),
    suma_edades = sum(P6040),
    edad_prom_hogar = round(suma_edades / cantidad_personas, 1)
  )


test_hogares <- left_join(test_hogares, caracteristicas_edad)
colnames(test_hogares)

# Imputar con la mediana
mediana_arriendo <- median(test_hogares$P5130, na.rm = TRUE)  # P5130 es el pago de arrendamiento

test_hogares <- test_hogares %>%
  mutate(P5130 = replace_na(P5130, mediana_arriendo))

summary(test_hogares$P5130)

# La de casa propia
test_hogares$casa_propia_paga <- ifelse(test_hogares$P5090 == 1, 1, 0)


test_personas %>%
  summarise_all(~sum(is.na(.))) %>% transpose()

# Personas en edad de trabajar
personas_Pet <- test_personas %>%
  group_by(id) %>%
  summarize(cantidad_pet_hogar = sum(Pet, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(cantidad_pet_hogar = replace(cantidad_pet_hogar, is.na(cantidad_pet_hogar), 0))

test_hogares <- left_join(test_hogares, personas_Pet)
colnames(test_hogares)

head(test_hogares[c("id", "cantidad_personas", "cantidad_pet_hogar")])

test_hogares %>%
  summarise_all(~sum(is.na(.))) %>% transpose()


summary(test_personas$P7495)
hist(test_personas$P7495)



mediana_pension <- median(test_personas$P7495, na.rm = TRUE)  # P7495 es si recibe pension/ arriendo

test_personas <- test_personas %>%
  mutate(P7495 = replace_na(P7495, mediana_pension))

test_personas$dummy_pension <- test_personas$P7495

dummy_pension_hogar <- test_personas %>%
  group_by(id) %>%
  summarize(pension_hogar = max(dummy_pension))

test_hogares <- left_join(test_hogares, dummy_pension_hogar)
colnames(test_hogares)

summary(test_hogares$pension_hogar)



test_hogares$pension_hogar<- ifelse(test_hogares$pension_hogar > 1, 1, ifelse(is.na(test_hogares$pension_hogar) | test_hogares$pension_hogar <= 1, 0, NA))



hist(test_personas$P6210s1) 
summary(test_personas$P6210s1) 

mediana_pension <- median(test_personas$P6210s1, na.rm = TRUE) # P6210s1 es educacion 

test_personas <- test_personas %>%
  mutate(P7495 = replace_na(P6210s1, mediana_pension))


educ_promedio <- test_personas %>%
  group_by(id) %>%
  summarize(
    cantidad_personas1 = n(),
    suma_educacion = sum(P6210s1),
    educ_prom_hogar = round(suma_educacion/ cantidad_personas1, 1)
  )

test_hogares <- left_join(test_hogares, educ_promedio)
colnames(test_hogares)

summary(test_hogares$educ_prom_hogar)

mean_educ<- mean(test_hogares$educ_prom_hogar, na.rm = TRUE)

test_hogares <- test_hogares %>%
  mutate(educ_prom_hogar = replace_na(educ_prom_hogar, mean_educ))



# Crear la variable dummy con 0 para missing values
test_personas$desempleados <- ifelse(test_personas$P6240 == 2 | is.na(test_personas$P6240), 1, 0)
summary(desempleados)


desempleados <- test_personas %>%
  group_by(id) %>%
  summarize(desempleados = max(desempleados))


test_hogares<-left_join(test_hogares, desempleados)
colnames(test_hogares)


mediana_desempleado <- median(test_hogares$desempleados, na.rm = TRUE)

test_hogares <- test_hogares %>%
  mutate(desempleados= replace_na(desempleados, mediana_desempleado))




mediana_pension <- median(test_hogares$pension_hogar, na.rm = TRUE)

test_hogares <- test_hogares %>%
  mutate(pension_hogar= replace_na(pension_hogar, mediana_pension))



##____________ RANDOM FOREST





library(caret)
library(tidymodels)

class_y <- class(train_hogares$Pobre)
print(class_y)

train_hogares$Pobre <- factor(train_hogares$Pobre)

train_hogares$Pobre <- factor(train_hogares$Pobre)
levels(train_hogares$Pobre) <- make.names(levels(train_hogares$Pobre))


split <- initial_split(train_hogares, prop = .75)
train_data <- training(split)
test_data  <- testing(split)

receta_test_adicional <- recipe(Pobre ~ edad_prom_hogar + Nper  + P5000 +  casa_propia_paga  + cantidad_pet_hogar + educ_prom_hogar + desempleados, data = train_data) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())


rf_grid_random <- grid_random(
  min_n(range = c(1, 15)),
  trees(range = c(20, 30)), size =5)


##

# Modelo Random Forest
rf_spec <- rand_forest(
  trees = tune(),
  min_n =   tune(),
  mtry =  sqrt(4)
) %>%
  set_mode("classification")


# Crear el flujo de trabajo
workflow_rf <- workflow() %>%
  add_recipe(receta_test_adicional) %>%
  add_model(rf_spec)

# Validación cruzada
df_fold <- vfold_cv(train_data, v = 4)

# Ajuste del modelo
tune_rf <- tune_grid(
  workflow_rf,
  resamples = df_fold, 
  grid = rf_grid_random
)





# Obtener el mejor hiper-parámetro
best_params_rf <- tune_rf %>% select_best(metric = "accuracy")
best_params_rf

# Entrenar el modelo con el mejor hiper-parámetro
best_rf_fit <- finalize_workflow(workflow_rf, best_params_rf) %>%
  fit(data = train_data)

# Predecir en el conjunto de prueba
test_data <- test_data %>%
  mutate(predicciones_rf = predict(best_rf_fit, test_data)$.pred_class)




test_data <- test_data %>%
  mutate(predicciones_rf=factor(predicciones_rf,levels=c(0,1),labels=c("No pobre","Pobre")))

test_data <- test_data %>%
  mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No pobre","Pobre")))

# Matriz de confusión
confusion_matrix_rf <- conf_mat(test_data, truth = Pobre, estimate = predicciones_rf)

print(confusion_matrix_rf )

# Calcular métricas
accuracy_rf <- accuracy(test_data, truth = Pobre , estimate = predicciones_rf)
accuracy_rf
## Accuracy del 81,7%


## PRediccion en FOREST (0s de 60,766 y 1s de 5402)

prediccion_forest_luc1 <- predict( best_rf_fit , new_data=test_hogares)
prediccion_forest_luc1
write.csv(test_hogares %>% select(id) %>% bind_cols(prediccion_forest_luc1),file = 'prediccion_forest_jd.csv')

summary(prediccion_forest_luc1)


##_______________________

p_load("adabag")


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

train_hogares$Pobre <- factor(train_hogares$Pobre)



class_adaboost <- train(Pobre ~ edad_prom_hogar + Nper + desempleados
                        + P5000 +  casa_propia_paga  + cantidad_pet_hogar
                        + educ_prom_hogar,
                        data=train_hogares,
                        method = "AdaBoost.M1",
                        trControl = ctrl,
                        tuneGrid=expand.grid(
                          mfinal = c(25,50,100),
                          maxdepth = c(1,2,3),
                          coeflearn = c('Breiman','Freund'))
)


##_____________________________


ada_grid_random <- grid_random(
  min_n(range = c(1, 20)),
  trees(range = c(10,90)),size = 10)


# Define la especificación del modelo
ab_spec <- boost_tree(
  trees = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("C5.0")  # Establece el motor como "C5.0"

# Crear el flujo de trabajo
workflow_ab <- workflow() %>%
  add_recipe(receta_test_adicional) %>%
  add_model(ab_spec)   


tune_ab <- tune_grid(
  workflow_ab,
  resamples = df_fold, 
  grid = ada_grid_random
)



best_params_ab <- tune_ab %>% select_best(metric = "accuracy")
best_params_ab

best_ab_fit <- finalize_workflow(workflow_ab, best_params_ab) %>%
  fit(data = train_data)

# Predecir en el conjunto de prueba
test_data <- test_data %>%
  mutate(predicciones_ab = predict(best_ab_fit, test_data)$.pred_class)

test_data <- test_data %>%
  mutate(predicciones_ab=factor(predicciones_ab,levels=c(0,1),labels=c("No pobre","Pobre")))


# Matriz de confusión
confusion_matrix_ab <- conf_mat(test_data, truth = Pobre, estimate = predicciones_ab)

# Imprimimos la matriz
print(confusion_matrix_ab)

accuracy_ab <- accuracy(test_data, truth = Pobre, estimate = predicciones_ab)
accuracy_ab

## Accuracy del 81,8%


## ADABOOSTRAP-------- LA MEJOR CON f1 DE 0,42

## PRediccion en Adaboost

prediccion_adaboost <- predict( best_ab_fit , new_data=test_hogares)
prediccion_adaboost
write.csv(test_hogares %>% select(id) %>% bind_cols(prediccion_adaboost),file = 'prediccion_adaboost.csv')

summary(prediccion_adaboost)

## MEJOR PREDICION ( O:57,165 Y 1:9003)



##______________      

receta_depto <- recipe(Pobre ~ edad_prom_hogar + Nper  + P5000 +  casa_propia_paga  + cantidad_pet_hogar + educ_prom_hogar + desempleados + pension_hogar + Depto, data = train_data) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())




## RIDGE Y LASSO

# Esto se utilizará para evaluar el rendimiento del modelo en diferentes subconjuntos de  datos durante la validación cruzada.
df_fold <- vfold_cv(train_hogares, v = 5)


ridge_recipe <- recipe(Pobre ~ edad_prom_hogar + Nper +  P5000 +  casa_propia_paga  + cantidad_pet_hogar + educ_prom_hogar + Depto, data = train_hogares) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())



# Definir una especificación de modelo lineal utilizando 'linear_reg'
ridge_spec <- logistic_reg(penalty = tune(), mixture = 0) %>%
  
  
  
  # Establecer el modo del modelo como 'regresión'
  set_mode("classification") %>%
  
  # Configurar el motor del modelo como 'glmnet', que se utiliza para modelos ridge
  #y lasso
  set_engine("glmnet")



# Crear un flujo de trabajo (workflow) para el modelo de regresión ridge

# Iniciar un flujo de trabajo utilizando 'workflow()'
ridge_workflow <- workflow() %>%
  
  # Agregar la receta de preprocesamiento de datos 'ridge_recipe' al flujo de trabajo
  add_recipe(ridge_recipe) %>%
  
  # Agregar la especificación del modelo de regresión ridge 'ridge_spec' al flujo de trabajo
  add_model(ridge_spec)

penalty_grid <- grid_regular(penalty(range = c(-4, 1)), levels = 30)
penalty_grid


metrics <- metric_set(accuracy)

tune_res <- tune_grid(
  ridge_workflow,         # El flujo de trabajo que contiene: receta y especificación del modelo
  resamples = df_fold,  # Folds de validación cruzada
  grid = penalty_grid,        # Grilla de valores de penalización
  metrics = metrics
)
tune_res

autoplot(tune_res)

collect_metrics(tune_res)

best_penalty <- select_best(tune_res, metric = "accuracy")
best_penalty


ridge_final <- finalize_workflow(ridge_workflow, best_penalty)


# Ajustar el modelo de regresión ridge utilizando los datos de entrenamiento
ridge_final_fit <- fit(ridge_final, data = train_data)
ridge_final_fit


test_data <- test_data %>%
  mutate(predicciones_ridge = predict(ridge_final_fit, test_data)$.pred_class)

test_data <- test_data %>%
  mutate(predicciones_ridge=factor(predicciones_ridge,levels=c(0,1),labels=c("No pobre","Pobre")))

# Matriz de confusión
confusion_matrix_ridge <- conf_mat(test_data, truth = Pobre, estimate = predicciones_ridge)

# Imprimimos la matriz
print(confusion_matrix_ridge)

accuracy_ridge <- accuracy(test_data, truth = Pobre, estimate = predicciones_ridge)
accuracy_ridge

## ACCURACY 81,2%

## por si necesitan   
##____________________         
augment(ridge_final_fit, new_data = train_hogares) %>%
  accuracy(truth = Pobre, estimate = .pred)
##________________________________


prediccion_ridge_pobre <- predict( ridge_final_fit , new_data=test_hogares)
prediccion_ridge_pobre
write.csv(test_hogares %>% select(id) %>% bind_cols(prediccion_ridge_pobre),file = 'prediccion_ridge_pobre.csv')

summary(prediccion_ridge_pobre)

## MUY POQUITOS 1S (1= 2572, 0S= 63,596)
## PREDIJO 0,21


##______________________##_______________________

## LASSO

lasso_recipe <- recipe(Pobre ~ edad_prom_hogar + Nper +  P5000 +  casa_propia_paga  + cantidad_pet_hogar + educ_prom_hogar + Depto, data = train_hogares) %>% 
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())


# Definir las especificaciones del modelo Lasso
lasso_spec <- 
  logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet") 

# Crear un flujo de trabajo que incluye la receta de preprocesamiento y el modelo Lasso
lasso_workflow <- workflow() %>%
  add_recipe(lasso_recipe) %>%
  add_model(lasso_spec)


# Utilizar 'grid_regular' para generar una cuadrícula de valores de penalización
penalty_grid <- grid_regular(penalty(range = c(-2, 2)), levels = 50)




# Realizar la búsqueda de hiperparámetros utilizando tune_grid
tune_res2 <- tune_grid(
  lasso_workflow,
  resamples = df_fold, 
  grid = penalty_grid,
  metrics = metric_set(accuracy)
)


tune_res3 <- tune_grid(
  lasso_workflow,
  resamples = df_fold, 
  grid = penalty_grid,
  metrics = metric_set(recall)
)

# Crear un gráfico para visualizar los resultados de la búsqueda 
#de hiperparámetros
autoplot(tune_res2)

best_penalty <- select_best(tune_res2, metric = "accuracy")
best_penalty

lasso_final <- finalize_workflow(lasso_workflow, best_penalty)

# Ajustar el modelo de regresión Lasso utilizando los datos de entrenamiento
lasso_final_fit <- fit(lasso_final, data = train_data)



test_data <- test_data %>%
  mutate(predicciones_lasso = predict(lasso_final_fit, test_data)$.pred_class)

test_data <- test_data %>%
  mutate(predicciones_lasso=factor(predicciones_lasso,levels=c(0,1),labels=c("No pobre","Pobre")))

# Matriz de confusión
confusion_matrix_ridge <- conf_mat(test_data, truth = Pobre, estimate = predicciones_lasso)

# Imprimimos la matriz
print(confusion_matrix_ridge)

accuracy_lasso <- accuracy(test_data, truth = Pobre, estimate = predicciones_lasso)
accuracy_lasso

## ACCURACY 80,9%

prediccion_lasso_pobre <- predict( lasso_final_fit , new_data=test_hogares)
prediccion_lasso_pobre
write.csv(test_hogares %>% select(id) %>% bind_cols(prediccion_lasso_pobre),file = 'prediccion_lasso_pobre.csv')

summary(prediccion_lasso_pobre)


##_______________________ REGRESION PREDICCION DE SALARIO Y LUEGO FILTRAR __________-------


library(yardstick) 

library(caret)
library(tidymodels)

class_y <- class(train_hogares$Lp)
print(class_y)


split <- initial_split(train_hogares, prop = .75)
train_data <- training(split)
test_data  <- testing(split)

receta_lp <- recipe(Lp ~ edad_prom_hogar + Nper  + P5000 +  casa_propia_paga  + cantidad_pet_hogar + educ_prom_hogar + desempleados + pension_hogar + Depto, data = train_data) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())


rf_grid_random <- grid_random(
  min_n(range = c(1, 30)),
  trees(range = c(15, 50)), size =5)


##

# Modelo Random Forest
rf_spec <- rand_forest(
  trees = tune(),
  min_n =   tune(),
  mtry =  sqrt(4)
) %>%
  set_mode("regression")


# Crear el flujo de trabajo
workflow_rf <- workflow() %>%
  add_recipe(receta_lp) %>%
  add_model(rf_spec)

# Validación cruzada
df_fold <- vfold_cv(train_data, v = 4)

# Ajuste del modelo
tune_rf <- tune_grid(
  workflow_rf,
  resamples = df_fold, 
  grid = rf_grid_random
)

tune_rf

# Obtener el mejor hiper-parámetro
best_params_rf <- tune_rf %>% select_best(metric ="rmse")
best_params_rf



# Entrenar el modelo con el mejor hiper-parámetro
best_rf_fit <- finalize_workflow(workflow_rf, best_params_rf) %>%
  fit(data = train_data)



# Predecir en el conjunto de prueba
test_data <- test_data %>%
  mutate(predicciones_rf = predict(best_rf_fit, test_data))

summary(test_data$ predicciones_rf) 

# Suponiendo que predicciones_rf y Lp son columnas en tu conjunto de datos
test_data$Pobre_rf <- ifelse(test_data$predicciones_rf > test_data$Lp, 0, ifelse(test_data$predicciones_rf < test_data$Lp, 1, NA))




test_data <- test_data %>%
  mutate(Pobre_rf=factor(Pobre_rf,levels=c(0,1),labels=c("No pobre","Pobre")))

test_data <- test_data %>%
  mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No pobre","Pobre")))

# Matriz de confusión
confusion_matrix_rf <- conf_mat(test_data, truth = Pobre, estimate = Pobre_rf)

print(confusion_matrix_rf )

# Calcular métricas
accuracy_rf <- accuracy(test_data, truth = Pobre , estimate = Pobre_rf)
accuracy_rf
## Accuracy del 30,7%




test_hogares$predict_lp <- predict( best_rf_fit , new_data=test_hogares)

# Suponiendo que predicciones_rf y Lp son columnas en tu conjunto de datos
test_hogares$Pobre_rf <- ifelse(test_hogares$predict_lp > test_hogares$Lp, 0, ifelse(test_hogares$predict_lp < test_hogares$Lp, 1, NA))




write.csv(test_hogares %>% select(id) %>% bind_cols(prediccion_forest_luc1),file = 'prediccion_forest_jd.csv')



## ( 0s de 16,555 y 1s de 49,613)

summary(factor(test_hogares$Pobre_rf))

write.csv(test_hogares %>% select(id) %>% bind_cols(test_hogares$Pobre_rf),file = 'prediccion_forest_linea_pobreza.csv')






## RANGER

tree_ranger_grid <- train(X.price.~X.bedrooms.+ X.month.+ terraza + parqueadero + casa + X.bathrooms.
                          ,
                          data=train,
                          method = "ranger",
                          trControl = fitControl,
                          metric="MAE",
                          tuneGrid=expand.grid(
                            mtry = c(1,2,3),
                            splitrule = "variance",
                            min.node.size = c(5,10,15))
)

tree_ranger_grid

# Predict 'Sale_Price' on the test dataset
pred3 <- predict(tree_ranger_grid, newdata = test)
pred3
write.csv(test %>% select(property_id) %>% bind_cols(pred3),file = 'random_forest_ranger_2.csv')




