############################################
# ANÁLISIS AIRBNB BARCELONA: DIAGNÓSTICO  #
############################################

# ========================
# INSTALACIÓN Y CARGA DE PAQUETES
# ========================

# Instalar paquetes solo si no están presentes
if (!require("qqplotr")) install.packages("qqplotr")

# Cargar librerías necesarias para análisis y visualización
library(readxl)
library(tidyverse)
library(dplyr)
library(leaps)
library(HH)
library(faraway)
library(glmnet)
library(qqplotr)
library(car)
library(lmtest)
library(nortest)
library(sandwich)
library(tseries)
library(caret)
# ========================
# CARGA Y EXPLORACIÓN INICIAL DE DATOS
# ========================

datos <- read_xlsx("airbnb_barcelona.xlsx")

# Inspeccionamos estructura general y distribución por barrios
head(datos)
str(datos)
sort(table(datos$barrio), decreasing = TRUE)  # barrios más frecuentes

# ========================
# LIMPIEZA DE DATOS
# ========================

# Verificamos cantidad de valores faltantes por variable
sort(colSums(is.na(datos)), decreasing = TRUE)

# Justificación:

# - `puntuacion` y `cod_postal` tienen muchos valores faltantes, y no las 
# consideraremos relevantes para el análisis.

# - `amenities` es una columna compleja de tratar (string largo), y la 
# información que aporta puede estar representada indirectamente por otras 
# variables.

# Eliminamos columnas con alta proporción de NAs o poco útiles
datos_limpios <- datos %>%
  dplyr::select(-amenities, -puntuacion, - cod_postal)

# Convertimos variables categóricas a factor para su posterior uso en modelos
datos_limpios <- datos_limpios %>%
  mutate(
    barrio = as.factor(barrio),
    tipo_habitacion = as.factor(tipo_habitacion)
  )

# ========================
# IMPUTACIÓN Y FILTRADO DE VALORES FALTANTES
# ========================

# Verificamos nuevamente los NAs después de eliminación de columnas
sort(colSums(is.na(datos_limpios)), decreasing = TRUE)

# Análisis:

# - Decidimos eliminar las filas que contienen NA en alguna variable, dado que 
# representan una proporción muy pequeña respecto al total de datos disponibles.

datos_limpios <- datos_limpios[complete.cases(datos_limpios), ]

# Confirmamos que ya no hay NAs
sort(colSums(is.na(datos_limpios)), decreasing = TRUE)

# ========================
# SELECCIÓN DE VARIABLES DE INTERÉS
# ========================

# Definimos el conjunto de variables que usaremos en el análisis posterior 
# (modelo de precios)

vars_imputar <- c("personas", "banios", "habitaciones", "barrio",
                  "camas", "precio_euros", "estancia_min", "tipo_habitacion")

# Filtramos solo las columnas seleccionadas
datos_limpios <- datos_limpios[, vars_imputar]

# ========================
# AGRUPACIÓN DE BARRIOS EN DISTRITOS
# ========================

# Justificación:

# - `barrio` tiene muchos niveles, lo que dificulta su uso como 
# variable explicativa.
# - Agrupar por `distrito` simplifica la interpretación del modelo y reduce la 
# dimensionalidad, manteniendo la información que consideramos importante 
# proporcioanda por la geografía.

datos_limpios <- datos_limpios %>%
  mutate(distritos = case_when(
    barrio %in% c("Ciutat Vella", "El Born", "El Gotic", "El Raval", "La Barceloneta", "Sant Pere/Santa Caterina") ~ "Ciutat Vella",
    barrio %in% c("Dreta de l'Eixample", "Eixample", "el Fort Pienc", "L'Antiga Esquerra de l'Eixample",
                  "La Nova Esquerra de l'Eixample", "La Sagrada Familia", "La Sagrada Fami­lia", "Sant Antoni") ~ "Eixample",
    barrio %in% c("El Poble-sec", "Sants-Montjuic") ~ "Sants-Montjuïc",
    barrio %in% c("La Maternitat i Sant Ramon", "Les Corts", "Pedralbes", "Les Tres Torres") ~ "Les Corts",
    barrio %in% c("El Putget i Farro", "Sant Gervasi - Galvany", "Sant Gervasi - la Bonanova", "Sarria", "Sarria-Sant Gervasi") ~ "Sarrià-Sant Gervasi",
    barrio %in% c("Camp d'en Grassot i Gracia Nova", "Gracia", "La Salut", "Vallcarca i els Penitents", 
                  "Vila de Gracia", "El Coll", "Glaries - El Parc") ~ "Gràcia",
    barrio %in% c("Can Baro", "Carmel", "El Baix Guinardo", "El Congres i els Indians", "La Font d'en Fargues", "Guinarda",
                  "Horta", "Horta-Guinarda", "La Teixonera", "Montbau", "Sant Genis dels Agudells", "Sant Geni­s dels Agudells",
                  "La Vall d'Hebron") ~ "Horta-Guinardó",
    barrio %in% c("La Guineueta - Canyelles", "La Prosperitat", "Nou Barris", "Porta", "Trinitat Nova",
                  "Turo de la Peira - Can Peguera", "Verdum - Los Roquetes", "Vilapicina i la Torre Llobeta", "La Trinitat Vella") ~ "Nou Barris",
    barrio %in% c("Sant Andreu", "Sant Andreu de Palomar", "La Sagrera", "Navas", "El Bon Pastor") ~ "Sant Andreu",
    barrio %in% c("Diagonal Mar - La Mar Bella", "El Besos i el Maresme", "El Poblenou", "La Verneda i La Pau",
                  "La Vila Olimpica", "Provencals del Poblenou", "Sant Marta", "Sant Marta de Provencals", "Sant Marta­ de Provencals",
                  "El Clot", "El Camp de l'Arpa del Clot") ~ "Sant Martí",
    TRUE ~ "Otros"
  )) %>%
  mutate(distritos = factor(distritos))

# Eliminamos `barrio` ya que ahora usamos `distritos` como variable geográfica
datos_limpios <- datos_limpios %>% dplyr::select(-barrio)

# Verificamos estructura final del conjunto de datos limpio
str(datos_limpios)


# ========================
# MODELO LINEAL MÚLTIPLE
# ========================

# Ajustamos un modelo lineal con todas las variables disponibles
mod_limpio <- lm(precio_euros ~ ., data = datos_limpios)

summary(mod_limpio)

car::Anova(mod_limpio)

# ========================
# SELECCIÓN DE VARIABLES
# ========================

# Definimos el modelo nulo (solo intercepto) para iniciar los métodos stepwise
modelo_base <- lm(precio_euros ~ 1, data = datos_limpios)

# --------------------------
# Stepwise
# --------------------------
modelo_stepwise <- step(modelo_base,
                        scope = formula(mod_limpio),
                        direction = "both",
                        trace = TRUE)

summary(modelo_stepwise)

# --------------------------
# Forward Selection
# --------------------------
modelo_forward <- step(modelo_base,
                       scope = formula(mod_limpio),
                       direction = "forward",
                       trace = TRUE)

summary(modelo_forward)
Anova(modelo_forward)

# --------------------------
# Backward Elimination 
# --------------------------
modelo_backward <- step(mod_limpio,
                        direction = "backward",
                        trace = TRUE)

summary(modelo_backward)

# --------------------------
# Stepwise con BIC
# --------------------------
n <- nrow(datos_limpios)

modelo_stepwise_bic <- step(modelo_base,
                            scope = formula(mod_limpio),
                            direction = "both",
                            k = log(n),  # Penalización BIC
                            trace = TRUE)

summary(modelo_stepwise_bic)

# ========================
# ANÁLISIS DE RESULTADOS
# ========================

# Todos los métodos de selección que probamos — stepwise, forward, backward y 
# hasta con BIC — terminaron eligiendo el modelo completo, o sea, con todas 
# las variables. Eso indica que todas las variables aportan información 
# importante para explicar el precio y que no hay ninguna que sea claramente 
# innecesaria según esos criterios.


#=========================== ANÁLISIS DE SUPUESTOS =============================#

# Detectamos valores atípicos extremos en las predicciones del modelo lineal. 
# Para mejorar la linealidad de los gráficos y evitar que estas observaciones distorsionen
# el análisis, decidimos eliminarlas iterativamente. 
# Como se trata de solo 5 observaciones sobre un total de ~16.700, su eliminación 
# tiene poco impacto general pero mejora la calidad del ajuste visual y diagnóstico.

# Creamos una copia de los datos originales para preservar la versión original.
datos_limpios_copy <- datos_limpios

# Eliminamos 5 observaciones con valores predichos mínimos de forma iterativa.
for (i in 1:5) {
  # Reajustar el modelo lineal con las variables numéricas (excluyendo las categóricas)
  mod_numerico <- lm(precio_euros ~ ., data = datos_limpios_copy %>%
                       dplyr::select(-tipo_habitacion, -distritos))
  
  # Calcular valores predichos
  datos_limpios_copy$predichos <- fitted(mod_numerico)
  
  # Identificar la observación con el valor predicho mínimo
  id_min <- which.min(datos_limpios_copy$predichos)
  
  # Eliminar esa observación del dataset
  datos_limpios_copy <- datos_limpios_copy[-id_min, ]
}

# Eliminamos las columnas auxiliares para evitar conflictos en análisis posteriores (VIF, etc.)
datos_limpios_copy <- datos_limpios_copy %>%
  dplyr::select(-predichos)

datos_limpios <- datos_limpios_copy

# Ajustamos nuevamente el modelo lineal final con las variables numéricas
mod_numerico <- lm(precio_euros ~ ., data = datos_limpios_copy %>%
                     dplyr::select(-tipo_habitacion, -distritos))

# ========================
# MULTICOLINEALIDAD
# ========================

# Realizamos un chequeo de multicolinealidad (VIF) y evaluamos la linealidad 
# para las variables numéricas

vif(mod_numerico)

# No se observan problemas de multicolinealidad, ya que todos los valores de VIF
# son menores a 5, lo cual está dentro de nuestro umbral aceptado.

#=============================#
#      ANÁLISIS: LINEALIDAD   #
#=============================#

# Calculamos los valores predichos y los residuos del modelo ajustado
datos_limpios_copy$predichos <- fitted(mod_numerico)
datos_limpios_copy$residuos  <- residuals(mod_numerico)

#------------------------------------------------------------------------------
# Gráfico básico: residuos vs valores predichos
# Permite evaluar si hay una tendencia no lineal (lo ideal es una nube sin forma).
#------------------------------------------------------------------------------
ggplot(datos_limpios_copy, aes(x = predichos, y = residuos)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, color = "red") +
  labs(
    x = "Valores predichos",
    y = "Residuos",
    title = "Residuos vs Predichos (básico)"
  ) +
  theme_minimal()

#------------------------------------------------------------------------------
# Gráfico mejorado con suavizado LOESS
# Evaluamos visualmente la linealidad general del modelo.
#------------------------------------------------------------------------------
ggplot(datos_limpios_copy, aes(x = predichos, y = residuos)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(
    x = "Valores ajustados",
    y = "Residuos",
    title = "Residuos vs Predichos (con suavizado)"
  ) +
  theme_minimal()

formula(mod_numerico)

#------------------------------------------------------------------------------
# Gráficos de componentes + residuos parciales (crPlots)
# Evalúan la relación lineal entre cada predictor numérico y la respuesta
#------------------------------------------------------------------------------
crPlot(mod_numerico, variable = 'banios',         pch = 16)
crPlot(mod_numerico, variable = 'habitaciones',   pch = 16)
crPlot(mod_numerico, variable = 'camas',          pch = 16)
crPlot(mod_numerico, variable = 'estancia_min',   pch = 16)
crPlot(mod_numerico, variable = 'personas',       pch = 16)

#------------------------------------------------------------------------------
# Conclusión: los gráficos no muestran patrones evidentes de no linealidad.
# Podemos asumir que el supuesto de linealidad se cumple.
#------------------------------------------------------------------------------

#==================================================#
#     SUPUESTO DE HOMOCEDASTICIDAD Y NORMALIDAD    #
#==================================================#

# Ajustamos un modelo más limpio con variables seleccionadas
mod_limpio <- lm(precio_euros ~ camas + banios + estancia_min + habitaciones + 
                   distritos + tipo_habitacion + personas, data = datos_limpios)

formula(mod_limpio)

# Calculamos los residuos estandarizados:

datos_limpios_copy$s_i <- rstandard(mod_limpio)  # Internamente studentizados
datos_limpios_copy$t_i <- rstudent(mod_limpio)   # Externamente studentizados

# Test de Breusch-Pagan para heterocedasticidad

bptest(mod_limpio)

# Resultado: p < 0.05 → Rechazamos la hipótesis nula de homocedasticidad
# => El modelo presenta heterocedasticidad (varianza no constante de los residuos)

# CORRECCIÓN CON ERRORES ESTÁNDAR ROBUSTOS     

# Usamos estimadores de varianza robustos ("sandwich") para corregir la 
# heterocedasticidad esto ajusta los errores estándar sin modificar los 
# coeficientes

# Coeficientes con errores estándar robustos
coeftest(mod_limpio, vcov = vcovHC(mod_limpio, type = "HC1"))

# Análisis de varianza robusto (Type III SS)
car::Anova(mod_limpio, vcov. = vcovHC(mod_limpio, type = "HC1"), type = 3)

# Cuando la varianza de los residuos no es constante, los errores estándar
# del modelo clásico pueden estar mal estimados. El estimador "sandwich" 
# corrige esto, haciendo que los tests de significancia sean válidos aun 
# cuando no se cumple la homocedasticidad.

# Conclusión: aunque el modelo no es homocedástico, podemos seguir con el 
# análisis utilizando estimaciones robustas para asegurar inferencias válidas.

#==========================================================#
#     SUPUESTO DE NORMALIDAD DE LOS ERRORES DEL MODELO     #
#==========================================================#

# Creamos un dataframe de trabajo con las variables del modelo sin transformaciones
datos_norm_check <- datos_limpios %>%
  dplyr::select(precio_euros, banios, habitaciones, camas, estancia_min, 
         tipo_habitacion, distritos, personas)

# Creamos una copia y calculamos residuos y valores predichos
datos_norm_check_copy <- datos_norm_check

# Ajustamos nuevamente el modelo para obtener residuos
mod_limpio <- lm(precio_euros ~ camas + banios + estancia_min + habitaciones + 
                   distritos + tipo_habitacion + personas, data = datos_limpios)

# Calculamos valores predichos y residuos
datos_norm_check_copy$predichos <- fitted(mod_limpio)
datos_norm_check_copy$residuos  <- residuals(mod_limpio)

# Calculamos residuos estandarizados:
datos_norm_check_copy$s_i <- rstandard(mod_limpio)  # Studentizados internos
datos_norm_check_copy$t_i <- rstudent(mod_limpio)   # Studentizados externos

#----------------------------------------------------------#
#     TESTS FORMALES DE NORMALIDAD SOBRE LOS RESIDUOS      #
#----------------------------------------------------------

# Hipótesis:
# H0: los errores se distribuyen normalmente
# H1: los errores no se distribuyen normalmente

# Test de Shapiro-Wilk
shapiro.test(datos_norm_check_copy$t_i)

# El test de Shapiro-Wilk no se puede aplicar porque la muestra es mayor a 
# 5000 observaciones, límite máximo del test. Por eso no se ejecuta 
# con >16.000 residuos.

# Test de Jarque-Bera (requiere tseries)
tseries::jarque.bera.test(datos_norm_check_copy$t_i)

# El test de Jarque-Bera rechaza la normalidad de los residuos (p < 2.2e-16),
# pero con una muestra grande, esta desviación no compromete la validez del modelo.

# Test de Kolmogorov-Smirnov contra la normal estándar
ks.test(datos_norm_check_copy$t_i, "pnorm")

# El test de Kolmogorov-Smirnov también rechaza la normalidad (p < 2.2e-16).

# Test de Anderson-Darling (requiere nortest)
nortest::ad.test(datos_norm_check_copy$t_i)

# El test de Anderson-Darling también rechaza la normalidad de los residuos (p < 2.2e-16),
# reforzando la evidencia de no normalidad en la muestra.

# CONCLUSIÓN:
# Los tests formales de normalidad (Jarque-Bera, Kolmogorov-Smirnov y Anderson-Darling)
# rechazan la hipótesis de normalidad debido al gran tamaño muestral.
# Sin embargo, dada la cantidad de datos (>16,000), estas desviaciones no afectan
# significativamente la validez inferencial del modelo.

#----------------------------------------------------------#
#     GRÁFICO Q-Q PLOT CON BANDAS DE CONFIANZA             #
#----------------------------------------------------------

# Visualizamos si los residuos siguen la distribución normal esperada
car::qqPlot(
  datos_norm_check_copy$t_i,
  main = "Q-Q Plot con bandas de confianza (95%)",
  envelope = 0.95,
  col.lines = "red"
)

# El Q-Q plot muestra que los residuos se comportan casi normales en el centro, 
# pero presentan desviaciones significativas en las colas, especialmente en la 
# derecha, indicando presencia de outliers o colas pesadas.

# Los residuos están bastante normales en el medio, pero en los extremos 
# (sobre todo en los valores altos) se salen un poco de lo esperado porque hay 
# más valores raros o extremos de lo normal. Esto puede hacer que algunos 
# análisis no funcionen tan bien, porque suelen pedir que los errores sean 
# normales. Pero si tenemos muchos datos, ese problema no pesa tanto porque 
# con más información las cosas se estabilizan. 


# Fijamos una semilla para que los resultados sean reproducibles (misma partición de los datos)
set.seed(123)

# Definimos los parámetros para la validación cruzada:
# method = "cv" → queremos usar validación cruzada
# number = 10 → vamos a usar 10-fold cross-validation (dividir el dataset en 10 partes)
control <- trainControl(method = "cv", number = 10)

# Entrenamos el modelo con validación cruzada usando la función train()
modelo_cv <- train(
  # Fórmula del modelo: regresión lineal sobre el logaritmo del precio
  # usando transformaciones logarítmicas para las variables numéricas
  precio_euros ~ 
    personas +
    camas +
    banios +
    habitaciones + 
    estancia_min + 
    distritos +
    tipo_habitacion,
  
  # Base de datos a utilizar
  data = datos_limpios,
  
  # Método de ajuste: modelo lineal
  method = "lm",
  
  # Parámetros de control definidos previamente (10-fold CV)
  trControl = control
)

# Mostramos el resumen de resultados de la validación cruzada
# Incluye métricas como RMSE (error cuadrático medio), R², etc.
print(modelo_cv)



