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
library(ggplot2)
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
library(mgcv)

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

mod_limpio <- lm(precio_euros ~ ., data = datos_limpios)

# ========================
# ANALISIS Y CONCLUSIONES
# ========================

# Boxplot por distritos
ggplot(data = datos_limpios) +
  geom_boxplot(aes(distritos, precio_euros, fill = distritos))

# El análisis comienza observando la distribución general de precios por 
# distrito. El boxplot deja claro que hay diferencias marcadas según la zona: 
# los barrios más céntricos o de mayor categoría como Eixample, Sarrià-Sant 
# Gervasi, Sant Martí y Sants-Montjuïc tienden a mostrar precios más elevados. 
# Eixample destaca especialmente por su gran dispersión, lo que indica una 
# amplia gama de alojamientos, desde económicos hasta extremadamente caros. 
# En contraste, zonas como Nou Barris, Horta-Guinardó y Sant Andreu presentan 
# precios medianos considerablemente más bajos y con menor variabilidad, 
# lo que sugiere una oferta más homogénea y accesible. Aun así, en casi todos los 
# distritos hay valores atípicos que superan los 1000 €, reflejando la 
# presencia de alojamientos de lujo en casi toda la ciudad.

# Estimaciones a partir del modelo
em_distritos <- emmeans(mod_limpio, ~ distritos)

# Predicciones en escala original con corrección de sesgo
em_distritos_euros <- summary(em_distritos, type = "response")  # incluye exp() y sigma²/2

ggplot(em_distritos_euros, aes(x = reorder(distritos, emmean), y = emmean)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  labs(
    x = "Distrito",
    y = "Precio estimado (€)",
    title = "Precio esperado por distrito"
  ) +
  coord_flip() +
  theme_minimal()

# Al usar un modelo ajustado para estimar el precio esperado por distrito, 
# se confirma la tendencia observada en el boxplot. Eixample, Sarrià-Sant 
# Gervasi y Sant Martí se mantienen como las zonas más caras, con precios 
# estimados por encima de los 90€, mientras que Sant Andreu, Nou Barris y 
# Les Corts figuran entre las más económicas. La ventaja del modelo es que 
# ajusta por otras variables, por lo que estas estimaciones reflejan mejor 
# las diferencias estructurales entre distritos. Además, los intervalos de 
# confianza son estrechos, lo que indica que las predicciones son 
# bastante precisas.


# Cálculo de media y mediana por distrito
precios_medianos <- datos_limpios %>%
  group_by(distritos) %>%
  summarise(mediana_precio = median(precio_euros, na.rm = TRUE))

precios_medianos$media <- datos_limpios |>
  with(tapply(precio_euros, distritos, mean))

p_m <- precios_medianos |>
  pivot_longer(cols = c(media, mediana_precio), names_to = "indicador", values_to = "valor")


# Gráfico de medianas reales por distrito
ggplot(precios_medianos, aes(y = reorder(distritos, mediana_precio), x = mediana_precio)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Mediana del precio real por distrito",
    x = "Distrito",
    y = "Mediana del precio (€)"
  ) +
  coord_flip() +
  theme_minimal()

# La mediana del precio por distrito ofrece una visión más robusta frente a 
# valores extremo y los resultados son consistentes con lo anterior: 
# los distritos más accesibles siguen siendo Nou Barris, Sant Andreu y 
# Horta-Guinardó (30–40€), mientras que Eixample ronda los 80€, 
# seguido de Sarrià-Sant Gervasi y Gràcia. 

# Gráfico comparando media y mediana
p_m$indicador <- factor(p_m$indicador, levels = c("media", "mediana_precio"))

ggplot(p_m, aes(x = reorder(distritos, valor), y = valor, fill = indicador)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs(
    x = "Distrito",
    y = "Precio (€)",
    title = "Media y mediana de precio por distrito"
  ) +
  coord_flip() +
  theme_minimal()

# Al comparar media y mediana, se visualiza el sesgo que generan los valores 
# extremos y en todos los distritos la media es superior a la mediana, 
# pero esa brecha es mucho más pronunciada en zonas como Eixample y Sarrià-Sant
# Gervasi. Esto indica que en esas zonas hay alojamientos particularmente caros 
# que elevan el promedio, pero, en barrios como Nou Barris o Sant Andreu, 
# la diferencia entre ambas medidas es mínima, lo que sugiere precios más 
# estables y homogéneos.

# Relación no lineal entre capacidad y precio por tipo de habitación
ggplot(datos_limpios, aes(x = personas, y = precio_euros, color = tipo_habitacion)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k = 5)) +
  labs(
    title = "Relación no lineal entre capacidad y precio por tipo de habitación",
    x = "Capacidad (personas)",
    y = "Precio (€)"
  ) +
  theme_minimal()

# Este gráfico muestra cómo varía el precio en función de la capacidad del 
# alojamiento, diferenciando por tipo de habitación. 
# En el caso de los apartamentos enteros, se nota una relación clara: a medida
# que aumenta la cantidad de personas que puede alojar, el precio también se 
# incrementa de forma progresiva. Esto refleja que los alojamientos más grandes 
# suelen estar mejor equipados y ser más costosos. Además, se observa bastante 
# dispersión, lo que indica una oferta muy variada dentro de este tipo.

# En cambio, las habitaciones privadas tienen una relación mucho más leve con 
# la capacidad. El precio se mantiene relativamente estable y, en capacidades 
# más altas, incluso tiende a disminuir levemente. Por su parte, las habitaciones 
# compartidas muestran una línea prácticamente plana, lo que indica que el 
# precio es casi independiente de cuántas personas puedan alojarse. 
# Esto sugiere que en este segmento más económico los precios están bastante 
# estandarizados.


# Distribución del precio por tipo de habitación y distrito
ggplot(datos_limpios, aes(x = tipo_habitacion, y = precio_euros, fill = tipo_habitacion)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.05) +
  facet_wrap(~ distritos) +
  labs(
    title = "Distribución del precio por tipo de habitación y distrito",
    x = "Tipo de habitación",
    y = "Precio (€)"
  ) +
  theme_minimal()

# Finalmente, al cruzar tipo de habitación con distrito, se refuerzan las 
# conclusiones anteriores. Los apartamentos enteros no solo tienen precios 
# más altos, sino también una mayor dispersión, especialmente en zonas como 
# Eixample, Gràcia y Sarrià-Sant Gervasi. Por otro lado, tanto las habitaciones 
# privadas como las compartidas presentan una distribución mucho más compacta, 
# con precios bajos y bastante uniformes entre distritos. Esto evidencia que 
# el tipo de alojamiento es, en muchos casos, más determinante que la ubicación
# para explicar la variabilidad del precio.