#############################
# ANÁLISIS AIRBNB BARCELONA #
#############################

# ============= #
# Cargar librerías
# ============= #
library(readxl)
library(dplyr)

# ============= #
# Importar datos
# ============= #
airbnb_barsa <- read_excel("airbnb_barcelona.xlsx")

# ================ #
# Exploración inicial
# ================ #
View(airbnb_barsa)
head(airbnb_barsa)
names(airbnb_barsa)
str(airbnb_barsa)

# ================= #
# LIMPIEZA DE DATOS #
# ================= #

# Variables a eliminar (innecesarias para el modelo)
airbnb_barsa <- select(airbnb_barsa, -id, -host_id, -cod_postal, -amenities)

# Mostrar columnas con valores faltantes
na_por_columna <- sapply(airbnb_barsa, function(x) sum(is.na(x)))
na_por_columna[na_por_columna > 0]

# Eliminar 'puntuacion' por gran cantidad de NAs
airbnb_barsa <- select(airbnb_barsa, -puntuacion)

# Conversión de 'tipo_habitacion' a factor con referencia "Shared room"
unique(airbnb_barsa$tipo_habitacion)
airbnb_barsa$tipo_habitacion <- relevel(
  as.factor(airbnb_barsa$tipo_habitacion), 
  ref = "Shared room"
)

# ================================ #
# Selección de variables del modelo
# ================================ #
vars_modelo <- c(
  "barrio", "latitud", "longitud", "personas", 
  "banios", "habitaciones", "camas", 
  "precio_euros", "estancia_min", "tipo_habitacion"
)

# Filtrado de filas completas (sin NAs en variables del modelo)
datos_sin_na <- airbnb_barsa[complete.cases(airbnb_barsa[, vars_modelo]), ]

# =================== #
# Manejo de atípicos
# =================== #

# Detectar y eliminar observación con estancia mínima atípica
outlier_index <- which.max(datos_sin_na$estancia_min)
datos_sin_na <- datos_sin_na[-outlier_index, ]

# Guardar versión limpia para análisis posterior
datos_sin_na_mod <- datos_sin_na
datos_des <- select(datos_sin_na, -barrio)

# ===================== #
# ANÁLISIS DESCRIPTIVO  #
# ===================== #

##############################
# ANÁLISIS DESCRIPTIVO UNIVARIADO
##############################

# Resumen general de las variables numéricas
summary(datos_des)

# ================== #
# Estadísticos Básicos
# ================== #

# === Medias ===
media_latitud      <- mean(datos_des$latitud)
media_longitud     <- mean(datos_des$longitud)
media_personas     <- mean(datos_des$personas)
media_banios       <- mean(datos_des$banios)
media_habitaciones <- mean(datos_des$habitaciones)
media_camas        <- mean(datos_des$camas)
media_precio       <- mean(datos_des$precio_euros)
media_estancia     <- mean(datos_des$estancia_min)

# === Varianzas ===
var_latitud      <- var(datos_des$latitud)
var_longitud     <- var(datos_des$longitud)
var_personas     <- var(datos_des$personas)
var_banios       <- var(datos_des$banios)
var_habitaciones <- var(datos_des$habitaciones)
var_camas        <- var(datos_des$camas)
var_precio       <- var(datos_des$precio_euros)
var_estancia     <- var(datos_des$estancia_min)

# === Coeficiente de Variación ===
cv_latitud      <- sd(datos_des$latitud) / media_latitud
cv_longitud     <- sd(datos_des$longitud) / media_longitud
cv_personas     <- sd(datos_des$personas) / media_personas
cv_banios       <- sd(datos_des$banios) / media_banios
cv_habitaciones <- sd(datos_des$habitaciones) / media_habitaciones
cv_camas        <- sd(datos_des$camas) / media_camas
cv_precio       <- sd(datos_des$precio_euros) / media_precio
cv_estancia     <- sd(datos_des$estancia_min) / media_estancia

# ===================== #
# ANÁLISIS BIVARIADO    #
# ===================== #

# Librería para visualización de correlaciones
library(corrplot)

# Selección de variables numéricas
vars_numericas <- datos_sin_na[, c(
  "latitud", "longitud", "personas", "banios",
  "habitaciones", "camas", "precio_euros", "estancia_min"
)]

# Matriz de correlación
matriz_cor <- cor(vars_numericas, use = "complete.obs")

# Visualización del mapa de correlaciones
corrplot(
  matriz_cor, 
  method = "color", 
  type = "upper", 
  tl.col = "black", 
  tl.srt = 45, 
  addCoef.col = "black"
)

# Comentarios sobre correlaciones observadas:
# - personas y habitaciones: 0.87 → más personas implican más habitaciones
# - personas y baños: 0.47 → más capacidad requiere más baños
# - habitaciones y camas: 0.82 → más habitaciones, más camas
# - personas y camas: 0.56 → más capacidad, más camas disponibles

# ====================== #
# MODELO LINEAL MÚLTIPLE #
# ====================== #

# =========================== #
# Modelo completo preliminar #
# =========================== #

mod_limpio <- lm(precio_euros ~ latitud + longitud + personas + banios + habitaciones +
                   camas + estancia_min - barrio - tipo_habitacion, data = datos_sin_na_mod)

# Comentario:
# El VIF del modelo, al incluir variables cuantitativas y cualitativas, 
# se vuelve complejo de interpretar. Por recomendación docente, 
# se decide quitar la variable tipo_habitacion para el análisis de multicolinealidad.

# Además, como hay menos de 10 variables, no se considera necesario aplicar
# técnicas como Backward, Forward, Stepwise o regularización (Lasso).

# ============================= #
# Supuesto: Multicolinealidad  #
# ============================= #

library(car)

# VIF para evaluar multicolinealidad
vif(mod_limpio)

# Correlaciones entre algunas variables numéricas
cor(datos_sin_na[5:8])

# Con VIF < 5 en todas las variables, se considera que no hay multicolinealidad problemática.

# Se construye nuevamente el modelo, ahora incluyendo tipo_habitacion
mod_limpio <- lm(precio_euros ~ latitud + longitud + personas + banios + habitaciones +
                   camas + estancia_min + tipo_habitacion - barrio, data = datos_sin_na_mod)

# ======================== #
# Supuesto: Linealidad     #
# ======================== #

library(ggplot2)

# Crear columnas de predicciones y residuos
datos_sin_na$predichos <- fitted(mod_limpio)
datos_sin_na$residuos  <- residuals(mod_limpio)

# Verificar fórmula
formula(mod_limpio)

# Gráfico residuos vs valores ajustados
ggplot(data = datos_sin_na, aes(x = predichos, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(
    x = "Valores ajustados",
    y = "Residuos",
    title = "Residuos vs Valores Ajustados"
  )

# Component + Residual Plots (gráficos para verificar linealidad individual)
crPlot(mod_limpio, variable = "latitud", pch = 16)
crPlot(mod_limpio, variable = "longitud", pch = 16)
crPlot(mod_limpio, variable = "tipo_habitacion", pch = 16)
crPlot(mod_limpio, variable = "personas", pch = 16)
crPlot(mod_limpio, variable = "banios", pch = 16)
crPlot(mod_limpio, variable = "habitaciones", pch = 16)
crPlot(mod_limpio, variable = "camas", pch = 16)
crPlot(mod_limpio, variable = "estancia_min", pch = 16)

# Comentarios:
# - 'estancia_min': no muestra linealidad clara. Cuidado si se transforma con log() por posibles ceros.
# - 'camas', 'habitaciones', 'banios', 'personas': muestran relación aproximadamente lineal con el precio.
# - 'latitud' y 'longitud': respetan adecuadamente la linealidad.
# - 'tipo_habitacion': no tiene sentido evaluar su linealidad al ser categórica.

# ============================ #
# Supuesto: Homoscedasticidad #
# ============================ #

#############################################
# SUPUESTO: HOMOSCEDASTICIDAD (VAR CONSTANTE)
#############################################

# Residuos studentizados externamente
datos_sin_na$t_i <- rstudent(mod_limpio)

# Gráficos de residuos vs predichos y variables explicativas
library(ggplot2)

# Función auxiliar para evitar repetir código
graficar_residuos <- function(x, varname, tipo = "puntos") {
  ggplot(datos_sin_na, aes_string(x = x, y = "t_i")) +
    {if (tipo == "puntos") geom_point() else geom_boxplot()} +
    geom_abline(slope = 0, intercept = 0, color = "red") +
    xlab(varname) +
    ylab("Residuos") +
    ggtitle(paste("Residuos vs", varname))
}

# Residuos vs predichos
graficar_residuos("predichos", "Valores ajustados")

# Residuos vs variables explicativas
graficar_residuos("latitud", "Latitud")
graficar_residuos("longitud", "Longitud")
graficar_residuos("tipo_habitacion", "Tipo de habitación", tipo = "boxplot")
graficar_residuos("personas", "Personas")
graficar_residuos("banios", "Baños")
graficar_residuos("habitaciones", "Habitaciones")
graficar_residuos("camas", "Camas")
graficar_residuos("estancia_min", "Estancia mínima")

# Comentarios observacionales:
# - "estancia_min" y "camas" muestran patrón de abanico → sospecha de heteroscedasticidad.
# - A mayor estancia mínima o cantidad de camas, mayor variabilidad en los residuos.

# Test de Breusch-Pagan para contrastar homoscedasticidad
library(skedastic)
breusch_pagan(mod_limpio)
# Resultado: se rechaza H0 → el modelo **no cumple** el supuesto de homoscedasticidad.

##############################
# SUPUESTO: NORMALIDAD DE LOS ERRORES
##############################

# Q-Q Plot manual
n <- nrow(datos_sin_na)
z_i <- qnorm(seq(n)/(n + 1))  # Cuantiles teóricos
qq <- data.frame(teoricos = z_i, empiricos = sort(datos_sin_na$t_i))

ggplot(qq, aes(x = teoricos, y = empiricos)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1.2) +
  xlab("Cuantiles teóricos") +
  ylab("Cuantiles empíricos") +
  ggtitle("Q-Q Plot manual")

# Q-Q plot con banda de confianza
library(qqplotr)
ggplot(qq, aes(sample = empiricos)) +
  stat_qq_band(fill = "pink") +
  stat_qq_line(color = "red") +
  stat_qq_point() +
  xlab("Cuantiles teóricos") +
  ylab("Cuantiles empíricos") +
  ggtitle("Q-Q Plot con banda de confianza")

# Histograma de residuos
hist(datos_sin_na$t_i, main = "Histograma de residuos studentizados",
     xlab = "Residuos studentizados", col = "lightblue", border = "white")

# Tests formales de normalidad
library(tseries)
tseries::jarque.bera.test(datos_sin_na$t_i)
ks.test(datos_sin_na$t_i, "pnorm", mean = 0, sd = 1)

# Resultado: ambos test rechazan H0 → **no se cumple** el supuesto de normalidad.

###########################################
# CONCLUSIONES DEL PRIMER ANÁLISIS LINEAL
###########################################

# No hay multicolinealidad preocupante (VIF < 5), aunque:
#    - camas, habitaciones y personas están correlacionadas (revisar si simplifican).
# No se cumple el supuesto de linealidad claramente para "estancia_min".
# No se cumple el supuesto de homoscedasticidad (varianza constante).
# No se cumple el supuesto de normalidad de los residuos.

###############################################
# TRANSFORMACIÓN DE LA VARIABLE RESPUESTA
###############################################

library(skedastic)

# Modelo con log(precio_euros) y sin tipo_habitacion (para VIF)
modp1 <- update(mod_limpio, log(precio_euros) ~ . - tipo_habitacion)

# ============================= #
# Supuesto: Multicolinealidad   #
# ============================= #

vif(modp1)  # Cumple con VIF < 5

# Ahora se vuelve a incluir tipo_habitacion
modp1 <- update(mod_limpio, log(precio_euros) ~ . + tipo_habitacion)

# ======================== #
# Supuesto: Linealidad     #
# ======================== #

datos_sin_na$predichos <- fitted(modp1)
datos_sin_na$residuos  <- residuals(modp1)

ggplot(datos_sin_na, aes(x = predichos, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(x = "Valores ajustados", y = "Residuos", title = "Residuos vs Predichos")

# Gráficos componente-residuo (crPlots)
cr_vars <- c("latitud", "longitud", "tipo_habitacion", "personas", 
             "banios", "habitaciones", "camas", "estancia_min")

for (var in cr_vars) {
  crPlot(modp1, variable = var, pch = 16)
}

# La linealidad sigue sin cumplirse claramente (especialmente estancia_min).

# ============================= #
# Supuesto: Homoscedasticidad  #
# ============================= #

breusch_pagan(modp1)
# Rechaza H0 → no hay homoscedasticidad.

# ============================ #
# Supuesto: Normalidad         #
# ============================ #

datos_sin_na$t_i <- rstudent(modp1)

tseries::jarque.bera.test(datos_sin_na$t_i)
ks.test(datos_sin_na$t_i, "pnorm", mean = 0, sd = 1)

# Se rechaza la normalidad de los residuos.

# ========================================= #
# CONCLUSIONES DEL SEGUNDO ANÁLISIS         #
# ========================================= #

# No hay multicolinealidad.
# No se cumple linealidad.
# No se cumple homoscedasticidad.
# No se cumple normalidad.
# Se decide transformar estancia_min.

###############################################
# TRANSFORMACIÓN DE LA VARIABLE estancia_min
###############################################

datos_sin_na$log_estancia_min <- log(datos_sin_na$estancia_min + 1)

modp2 <- update(modp1, . ~ . - estancia_min + log_estancia_min)

# ============================= #
# Supuesto: Multicolinealidad  #
# ============================= #

vif(modp2)  # Se mantiene bajo → OK

# ======================== #
# Supuesto: Linealidad     #
# ======================== #

datos_sin_na$predichos <- fitted(modp2)
datos_sin_na$residuos  <- residuals(modp2)

ggplot(datos_sin_na, aes(x = predichos, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(x = "Valores ajustados", y = "Residuos", title = "Residuos vs Predichos")

# Nuevos gráficos crPlot
cr_vars <- c("latitud", "longitud", "tipo_habitacion", "personas", 
             "banios", "habitaciones", "camas", "log_estancia_min")

for (var in cr_vars) {
  crPlot(modp2, variable = var, pch = 16)
}

# Aunque mejora, aún no se ajusta del todo a linealidad.

# ============================= #
# Supuesto: Homoscedasticidad  #
# ============================= #

breusch_pagan(modp2)
# Sigue sin cumplir homoscedasticidad

# ============================ #
# Supuesto: Normalidad         #
# ============================ #

datos_sin_na$t_i <- rstudent(modp2)

tseries::jarque.bera.test(datos_sin_na$t_i)
ks.test(datos_sin_na$t_i, "pnorm", mean = 0, sd = 1)

# No se cumple la normalidad

# ========================================= #
# CONCLUSIONES DEL TERCER ANÁLISIS          #
# ========================================= #

# No hay multicolinealidad.
# La linealidad aún no es clara.
# No se cumple homoscedasticidad.
# No se cumple normalidad.

######################################
# TRANSFORMACIÓN TOTAL DEL MODELO
######################################

# Filtrado de datos válidos
datos_filtrados <- datos_sin_na_mod %>%
  filter(
    precio_euros > 0,
    latitud > 0,
    longitud > 0,
    personas > 0,
    banios >= 0,
    habitaciones >= 0,
    camas >= 0,
    estancia_min >= 0
  )

# Dataset logarítmico
log_datos_sin_na <- datos_filtrados %>%
  mutate(
    log_precios_euro   = log(precio_euros),
    log_latitud        = log(latitud),
    log_longitud       = log(longitud),
    log_personas       = log(personas),
    log_banios         = log(banios + 1),
    log_habitaciones   = log(habitaciones + 1),
    log_camas          = log(camas + 1),
    log_estancia_min   = log(estancia_min + 1)
  ) %>%
  select(
    log_precios_euro, log_latitud, log_longitud, log_personas,
    log_banios, log_habitaciones, log_camas, log_estancia_min,
    tipo_habitacion, barrio
  )

# Modelo con variables transformadas (sin barrio)
log_mod <- lm(log_precios_euro ~ . - barrio, data = log_datos_sin_na)

# ========================== #
# Supuesto Multicolinealidad #
# ========================== #

vif(log_mod)

# cumple con no haber multicolinealidad

log_mod <- lm(log_precios_euro ~ . - barrio, data = log_datos_sin_na_mod)

# =================== #
# Supuesto linealidad #
# =================== #

log_datos_sin_na$predichos <- fitted(log_mod)
log_datos_sin_na$residuos <- residuals(log_mod)

# graficos de predichos vs residuos

ggplot(data = log_datos_sin_na, aes(x = log_mod$fitted.values, y = resid(log_mod))) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(x = "Valores ajustados", y = "Residuos", title = "Residuos vs Predichos")

crPlot(log_mod, variable = 'log_latitud', pch = 16)
crPlot(log_mod, variable = 'log_longitud', pch = 16)
crPlot(log_mod, variable = 'tipo_habitacion', pch = 16)
crPlot(log_mod, variable = 'log_personas', pch = 16)
crPlot(log_mod, variable = 'log_banios', pch = 16)
crPlot(log_mod, variable = 'log_habitaciones', pch = 16)
crPlot(log_mod, variable = 'log_camas', pch = 16)
crPlot(log_mod, variable = 'log_estancia_min', pch = 16)

# La nube de puntos se distribuye aproximadamente alrededor de la línea roja horizontal (y = 0).
# No se observa una curva clara o patrón sistemático en la forma general del gráfico.
# La línea azul (suavizada con loess) es bastante recta en la mayoría del rango de valores ajustados.
# Lo que podría levantar sospechas:
# En los extremos (valores ajustados muy altos o muy bajos), la línea azul se curva 
# un poco hacia arriba, lo que sugiere una posible leve no linealidad en las colas.
# Sin embargo, esto podría deberse a pocos datos en esos rangos extremos, 
# lo cual no compromete seriamente el modelo.

# ========================== #
# Supuesto Homoscedasticidad #
# ========================== #

breusch_pagan(log_mod)

# =================== #
# Supuesto Normalidad #
# =================== #

log_datos_sin_na$t_i <- rstudent(log_mod) # studentizados EXTERNAMENTE

n <- nrow(log_datos_sin_na)

# para construir el Q-Q plot
z_i <- qnorm(seq(n)/(n + 1))
qq <- data.frame(teoricos = z_i,
                 empiricos = sort(datos_sin_na$t_i))

ggplot(qq, aes(x = teoricos, y = empiricos)) +
  geom_point() +
  xlab('Cuantiles teoricos') +
  ylab('Cuantiles empiricos') +
  geom_abline(slope = 1, intercept = 0, col = 2, size = 1.5)

# alternativamente
ggplot(data = qq, aes(sample = empiricos)) +
  stat_qq_band(fill = 2) +
  stat_qq_line(col = 2) +
  stat_qq_point() +
  xlab("Cuantiles teoricos")+
  ylab("Cuantiles empiricos")

# histogramas
hist(rstudent(mod_limpio))

tseries::jarque.bera.test(log_datos_sin_na$t_i)
ks.test(log_datos_sin_na$t_i, 'pnorm', mean = 0, sd = 1)

# ================================= # 
# Conclusiones del Cuarto analicis  #
# ================================= #

# El actual modelo cumple con los supuesto de Multicolinealidad, y Linealidad. 
# sigue sin cumplir los supuestos de Homoscedasticidad y Normalidad.

# ======================================================================================= #

# =============== # 
# Datos Atipicos  #
# =============== #

# ============================================ #
# MEDIDAS DE INFLUENCIA EN MODELO DE REGRESIÓN #
# ============================================ #

# Suponemos que ya tenés tu modelo lineal creado:
# mod_limpio <- lm(precio_euros ~ ..., data = datos_sin_na_mod)

# ======================================= #
# Cálculo de leverage y distancia de Cook #
# ======================================= #

# Leverage (hᵢ) mide qué tan "extrema" es una observación en el espacio de predictores
h_i <- influence(log_mod)$hat

# Distancia de Cook (Dᵢ) mide la influencia total de una observación en los coeficientes del modelo
D_i <- cooks.distance(log_mod)

# Armamos un dataframe con el índice de observaciones y las medidas
df <- data.frame(
  i = 1:nrow(log_datos_sin_na_mod),  # índice de observación
  h_i = h_i,                     # leverage
  D_i = D_i                      # distancia de Cook
)

# ==========================
# Umbrales de referencia
# ==========================

# Cantidad de observaciones (n) y cantidad de parámetros del modelo (p)
n <- nrow(log_datos_sin_na_mod)
p <- length(coef(log_mod))  # incluye intercepto

# Umbral de leverage: regla práctica 2p/n
umbral_hi <- 2 * p / n

# Umbral de Cook: regla práctica 4/n
umbral_cook <- 4 / n

# ======================== #
# Gráfico de leverage (hᵢ) #
# ======================== #

ggplot(df, aes(x = i, y = h_i)) +
  geom_point(color = "black") +  # puntos de leverage
  geom_segment(aes(xend = i, yend = 0), alpha = 0.5) +  # líneas desde el eje
  geom_abline(slope = 0, intercept = umbral_hi, col = "red", linetype = "dashed") +  # umbral
  labs(title = "Leverage por observación",
       x = "Índice de observación",
       y = expression(h[i])) +
  theme_minimal()

# ================================= #
# Gráfico de distancia de Cook (Dᵢ) #
# ================================= #

ggplot(df, aes(x = i, y = D_i)) +
  geom_point(color = "black") +  # puntos de distancia de Cook
  geom_segment(aes(xend = i, yend = 0), alpha = 0.5) +
  geom_abline(slope = 0, intercept = umbral_cook, col = "blue", linetype = "dashed") +  # umbral
  labs(title = "Distancia de Cook por observación",
       x = "Índice de observación",
       y = expression(D[i])) +
  theme_minimal()

# influence(mod_limpio) devuelve varias métricas de influencia para cada observación."$hat" 
# extrae los valores de leverage (hᵢ), que indican cuánto influye una observación en su valor 
# predicho.56 Valores altos de hᵢ indican que esa observación está "lejos" del promedio 
# de las X (variables explicativas).
# cooks.distance() calcula la distancia de Cook (Dᵢ), que mide cuánto cambiarían 
# los coeficientes del modelo si eliminaras esa observación.
# Es una medida de influencia global: combina apalancamiento (hᵢ) y tamaño del residuo.
# Son reglas prácticas que nos dan límites de referencia:
# Creamos un dataframe df con: 
# i: número de fila u observación.
# h_i: leverage de esa observación.
# D_i: distancia de Cook de esa observación.
# Este dataframe lo usaremos para graficar fácilmente.
# n: cantidad de observaciones (filas).
# p: número de parámetros en el modelo (coef() incluye intercepto).
# Si hᵢ > 2p/n: observación con alto leverage.
# Si Dᵢ > 4/n: observación con alta influencia global (según Cook)
# aes(x = i, y = h_i): mapea el índice de observación al eje X y su leverage al eje Y.
# geom_point(): puntos para cada observación.
# geom_segment(...): líneas verticales desde el eje hasta el punto (estilo gráfico).
# geom_abline(...): línea horizontal en el umbral 2p/n (en rojo y línea punteada).
# labs(): etiquetas del gráfico.
# theme_minimal(): estilo visual limpio.

# Creamos un data frame para graficar
df_outliers <- data.frame(
  observacion = 1:nrow(log_datos_sin_na),
  residuos = log_datos_sin_na$t_i
)

# Identificamos observaciones fuera del umbral |t_i| > 3
df_outliers$atipico <- abs(df_outliers$residuos) > 3

# Gráfico
ggplot(df_outliers, aes(x = observacion, y = residuos)) +
  geom_point(aes(color = atipico), size = 2) +
  scale_color_manual(values = c("FALSE" = "purple", "TRUE" = "orange")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = c(-3, 3), color = "purple4", size = 1.2) +
  labs(
    x = "Observación",
    y = "Residuos studentizados",
    title = "Análisis de observaciones atípicas"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

###########################################
# DETECCIÓN DE OBSERVACIONES ATÍPICAS
###########################################

# Identificar observaciones atípicas: |t_i| > 3
log_datos_sin_na$atipica <- abs(log_datos_sin_na$t_i) > 3

# Contar cuántas observaciones atípicas hay
sum(log_datos_sin_na$atipica)

# Ver índices de observaciones atípicas
which(log_datos_sin_na$atipica)

# Ver detalles de las observaciones atípicas
log_datos_sin_na[log_datos_sin_na$atipica, ]

##############################################
# USO DE COVARIANZA ROBUSTA: LIBRERÍA sandwich
##############################################

library(sandwich)
library(lmtest)

# Documentación útil
# help(sandwich)
# help(vcovHC)

# Ejemplo simple
x <- sin(1:100)
y <- 1 + x + rnorm(100)

# Modelo lineal
fm <- lm(y ~ x)

# Matriz de covarianza robusta (tipo HC3 por defecto)
vcovHC(fm)

# Matriz de covarianza usual (homoscedástica)
vcovHC(fm, type = "const")

# Matriz de covarianza clásica
vcov(fm)

# Comparación directa con cálculo manual de sigma^2
sigma2 <- sum(residuals(fm)^2) / df.residual(fm)
sigma2 * solve(crossprod(cbind(1, x)))

# Test de significancia robusto (HC0)
coeftest(fm, vcov = vcovHC(fm, "HC0"))

# Aplicación al modelo final log_mod con errores robustos
# Debido a que no se logró resolver la heterocedasticidad
coeftest(log_mod, vcov = vcovHC(log_mod, "HC0"))
summary(log_mod)

log_mod <- update(log_mod, . ~ . - log_camas)
coeftest(log_mod, vcov = vcovHC(log_mod, "HC0"))
summary(log_mod)
#########################################
# VISUALIZACIÓN GEOGRÁFICA DE BARCELONA
#########################################

ggplot(datos_sin_na, aes(x = longitud, y = latitud, color = barrio)) +
  geom_point(alpha = 0.6, size = 1.8) +
  labs(
    title = "Ubicación geográfica de los alojamientos según barrio",
    x = "Longitud",
    y = "Latitud",
    color = "Barrio"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold")
  )

###################################
# CLASIFICACIÓN SOCIOECONÓMICA
###################################

# Barrios de nivel socioeconómico alto
barrios_alto <- c(
  "Pedralbes", "Les Tres Torres", "Sant Gervasi - Galvany",
  "Sarria-Sant Gervasi", "Sant Gervasi - la Bonanova",
  "El Putget i Farro", "Dreta de l'Eixample",
  "L'Antiga Esquerra de l'Eixample"
)

# Barrios de nivel socioeconómico medio
barrios_medio <- c(
  "Gracia", "Vila de Gracia", "Camp d’en Grassot i Gracia Nova",
  "Eixample", "La Nova Esquerra de l'Eixample",
  "La Sagrada Família", "El Clot", "Sant Antoni",
  "El Poble-sec", "La Barceloneta", "El Poblenou",
  "La Vila Olimpica", "El Born", "Sants-Montjuic",
  "Horta", "La Salut", "Vallcarca i els Penitents"
)

# Barrios de nivel socioeconómico bajo
barrios_bajo <- c(
  "El Raval", "Nou Barris", "La Trinitat Vella",
  "El Besos i el Maresme", "El Bon Pastor",
  "Verdum – Los Roquetes", "Porta", "Carmel",
  "El Congres i els Indians", "Can Baro",
  "La Verneda i La Pau", "La Guineueta – Canyelles",
  "Trinitat Nova", "Turo de la Peira – Can Peguera"
)

# Asignar nivel socioeconómico
datos_sin_na <- datos_sin_na %>%
  mutate(nivel_ingreso = case_when(
    barrio %in% barrios_alto ~ "Alto",
    barrio %in% barrios_bajo ~ "Bajo",
    barrio %in% barrios_medio ~ "Medio",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(nivel_ingreso))

# Cargar paquetes
library(ggmap)
library(ggplot2)
library(dplyr)

# Registrar API Key de Stadia Maps (usás la tuya)
register_stadiamaps(key = "efb988ba-20b3-4815-afab-5a633212ca85")

# 🗺️ Descargar mapa base de Barcelona
mapa_bcn <- get_stadiamap(
  bbox = c(left = 2.11, bottom = 41.35, right = 2.23, top = 41.46),
  zoom = 13,
  maptype = "stamen_toner_lite"
)

# Asegurarse de tener datos con nivel_ingreso
datos_plot <- datos_sin_na %>%
  filter(!is.na(latitud), !is.na(longitud), !is.na(nivel_ingreso))

# Graficar con mejoras
grafico <- ggmap(mapa_bcn) +
  geom_point(data = datos_plot,
             aes(x = longitud, y = latitud, fill = nivel_ingreso),
             shape = 21, size = 1.8, alpha = 0.7, color = "black", stroke = 0.2) +
  scale_fill_manual(
    name = "Nivel socioeconómico",
    values = c("Alto" = "forestgreen", "Medio" = "gold", "Bajo" = "red"),
    breaks = c("Alto", "Medio", "Bajo"),
    labels = c("Alto", "Medio", "Bajo")
  ) +
  labs(
    title = "Mapa socioeconómico de alojamientos en Barcelona",
    subtitle = "Distribución según el nivel de ingreso del barrio",
    x = "Longitud", y = "Latitud"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11)
  )

# Exportar como imagen PNG de alta calidad
ggsave("mapa_nivel_socio_bcn.png", plot = grafico, width = 9, height = 7, dpi = 300)


ibrary(caret)

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
  log_precios_euro ~ 
    log_latitud + 
    log_longitud + 
    log_personas + 
    log_banios +
    log_habitaciones + 
    log_estancia_min + 
    tipo_habitacion,
  
  # Base de datos a utilizar
  data = log_datos_sin_na,
  
  # Método de ajuste: modelo lineal
  method = "lm",
  
  # Parámetros de control definidos previamente (10-fold CV)
  trControl = control
)

# Mostramos el resumen de resultados de la validación cruzada
# Incluye métricas como RMSE (error cuadrático medio), R², etc.
print(modelo_cv)
