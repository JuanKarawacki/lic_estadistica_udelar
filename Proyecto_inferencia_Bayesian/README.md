# Parcial 1 – MCMC y modelos bayesianos (Metropolis–Hastings y Stan)

## Descripción
Este proyecto corresponde al **Parcial 1** de un curso de inferencia bayesiana / métodos
computacionales. El trabajo implementa y analiza:

- Variantes del algoritmo **Metropolis–Hastings** con propuestas normales.
- Construcción y diagnóstico de cadenas MCMC (trace plots, histogramas/densidades).
- Simulación y diagnóstico con **RStan** en modelos conjugados:
  - Modelo **Normal–Normal**
  - Modelo **Beta–Geométrico**

El informe está desarrollado en **R Markdown** y genera salidas en **PDF** y **HTML**.

## Contenidos principales

### Ejercicio 1 — Metropolis–Hastings con propuesta Normal
- Se implementa `one_mh_iteration_normal()` con propuesta:
  \[
  \mu' \mid \mu \sim N(\mu, s^2)
  \]
- Se analiza el efecto de distintos valores de `s` sobre:
  - la propuesta (`proposal`)
  - la tasa de aceptación (`alpha`)
  - el siguiente estado (`next_stop`)

### Ejercicio 2 — Recorrido MH y elección de `s`
- Se implementa `mh_tour_normal()` para generar cadenas de \(\mu\).
- Se comparan trazas e histogramas bajo diferentes configuraciones:
  - 20 iteraciones con `s = 0.01` y `s = 10`
  - 1000 iteraciones con `s = 0.01` y `s = 10`
- Se discute el compromiso exploración/aceptación y se propone un `s` razonable
  (valor intermedio) respaldado por trazas.

### Ejercicio 3 — Modelo Normal–Normal con Stan
- Modelo:
  \[
  Y_i \mid \mu \sim N(\mu, 8^2), \quad \mu \sim N(-14, 2^2)
  \]
- Se ajusta con **RStan** (4 cadenas, 10000 iteraciones).
- Se generan trace plots y density plots por cadena.
- Se especifica la **posterior exacta** por conjugación y se compara con la aproximación MCMC.

### Ejercicio 4 — Modelo Beta–Geométrico con Stan
- Modelo:
  \[
  Y \mid \theta \sim \text{Geométrica}(\theta), \quad \theta \sim \text{Beta}(\alpha,\beta)
  \]
- Se implementa el modelo en Stan y se simula la posterior.
- Se generan trace plots y density plots por cadena.
- Se deriva la **posterior exacta** (conjugación Beta) y se compara con la MCMC.

## Herramientas y tecnologías
- Lenguaje: **R**
- Reporte: **R Markdown** (`.Rmd`)
- MCMC / Bayes: `bayesrules`
- Manipulación y gráficos: `tidyverse`, `ggplot2`, `janitor`
- Reporte/tablas: `knitr`, `kableExtra`
- Stan: `rstan`, `bayesplot`

## Estructura del proyecto
/report
  Parcial1.Rmd          # informe principal (R Markdown)
/src
  (opcional)            # funciones auxiliares si se separan del informe
/output
  (opcional)            # figuras/tablas exportadas
## Cómo reproducir
1. Clonar el repositorio.
2. Abrir `report/Parcial1.Rmd` en RStudio.
3. Instalar dependencias (si no están):
   - `install.packages(c("bayesrules","tidyverse","janitor","knitr","kableExtra","rstan","bayesplot","gridExtra","ggplot2"))`
4. Compilar el informe a PDF/HTML (Knit).
   - Para PDF se utiliza `xelatex` (TinyTeX/LaTeX requerido).

## Estado del proyecto
Proyecto académico finalizado.

## Autor
Juan M. Karawacki  
(Parcial académico)
