# Análisis de la variación de precios de alquileres en Barcelona (Airbnb)
Trabajo final – Modelos Lineales

## Descripción
Trabajo final del curso **Modelos Lineales**. El proyecto construye y evalúa modelos de
**regresión lineal múltiple** para explicar y predecir el **precio de alquiler** en Airbnb en la
ciudad de **Barcelona (España)**. Se realiza un análisis exploratorio, selección de variables,
comparación de especificaciones alternativas y un diagnóstico exhaustivo de supuestos
(linealidad, homoscedasticidad, normalidad, multicolinealidad e influencia), incluyendo
estimación robusta ante heterocedasticidad.

## Objetivos
- Construir un modelo de regresión lineal múltiple que explique la variación del precio de alquiler.
- Identificar variables relevantes y lograr un modelo **parsimonioso** sin perder capacidad explicativa.
- Evaluar y corregir (cuando corresponda) el incumplimiento de supuestos del modelo lineal clásico.
- Comparar enfoques de modelado espacial:
  - Modelo con **distritos** (categórico)
  - Modelo con **latitud y longitud** (cuantitativo)
- Incorporar **estimación robusta** para tratar heterocedasticidad.

## Datos
- Fuente: dataset provisto por el curso (Airbnb Barcelona).
- Variable respuesta: **precio de alquiler** (Airbnb).
- Variables explicativas (según disponibilidad en el dataset):
  - Variables de ubicación: **distrito** / **latitud** / **longitud**
  - Características del alojamiento (incluye tratamiento especial de **amenities**)
  - Variables identificatorias (evaluadas y tratadas según su rol)
  - Presencia y análisis de **datos faltantes (NA)**

> Nota: si el dataset no se incluye en el repositorio, ver `data/README.md` para instrucciones.

## Metodología

### 1) Análisis exploratorio (EDA)
- Descripción del dataset y medidas de resumen.
- Evaluación inicial de relaciones precio–covariables.
- Identificación de patrones de faltantes.

### 2) Preparación de variables
- **Amenities**: construcción/tratamiento como variable compleja.
- Análisis y manejo de **NA**.
- Revisión de variables identificatorias.

### 3) Modelado
Se ajustan y comparan múltiples modelos, destacando dos enfoques:
- **Modelo categórico (Distritos)**: ubicación como factor.
- **Modelo cuantitativo (Latitud/Longitud)**: ubicación como coordenadas.

### 4) Selección de variables
- Aplicación de mecanismos de selección de variables y preparación del modelo final.
- Búsqueda de un equilibrio entre interpretabilidad y desempeño.

### 5) Diagnóstico del modelo
Se evalúan supuestos clásicos y problemas frecuentes:
- **Multicolinealidad**
- **Linealidad**
- **Homoscedasticidad**
- **Normalidad de residuos**
- **Observaciones atípicas e influencia**

### 6) Ajustes y robustez
- Transformaciones para mejorar supuestos (modelo transformado).
- **Estimación robusta ante heterocedasticidad** y comparación con el ajuste clásico.

## Resultados principales (resumen)
- Se comparan y discuten los resultados de:
  - Modelo con **distritos**
  - Modelo con **latitud/longitud**
- Se presenta un **modelo final** (con énfasis en latitud/longitud) tras transformaciones y diagnóstico.
- Se reportan hallazgos sobre:
  - cumplimiento/violación de supuestos
  - necesidad de estimación robusta
  - implicancias interpretativas para el precio del alquiler

## Estructura del proyecto
/src        # scripts de limpieza, EDA, modelado y diagnósticos
/report     # informe final (R Markdown / Quarto) + PDF
/data       # dataset (si se incluye) o instrucciones de descarga/uso
/output     # tablas y figuras exportadas (si aplica)
## Cómo reproducir
1. Clonar el repositorio.
2. Colocar el dataset en `/data` (si no está incluido) siguiendo `data/README.md`.
3. Abrir el proyecto en RStudio.
4. Ejecutar scripts en `/src` o compilar el informe en `/report`.

## Herramientas y tecnologías
- R / RStudio
- Paquetes típicos para:
  - manipulación: `tidyverse`
  - modelado: `stats` (lm) y utilidades complementarias
  - diagnóstico: (según el código del proyecto)
  - robustez: estimación robusta ante heterocedasticidad (según implementación)

## Autores
Lucas Giúdice  
Juan Manuel Karawacki  
Bruno Pintos  

## Docentes
Fernando Massa  
Ignacio Campón  

## Fecha
30 de junio de 2025
