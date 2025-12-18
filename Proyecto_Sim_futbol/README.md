# Simulación de Temporadas de Fútbol (Liga ficticia)

Proyecto de **estadística computacional** que construye un simulador de
una liga de fútbol (20 equipos, 38 fechas ida y vuelta), incorporando
mecanismos probabilísticos y económicos para estudiar:

- **Concentración de títulos** y formación de “dinastías”.
- Relación **presupuesto ↔ rendimiento** (puntos) y eficiencia de gasto.
- Efectos de **lesiones**, **titulares/suplentes** y azar del juego.
- Calibración de la dinámica goleadora para replicar un objetivo real
  de goles por partido.

---

## Resumen del proyecto

Se simulan múltiples temporadas de una liga ficticia a partir de reglas
explícitas de juego: generación de goles por jugador, lesiones
dependientes del tiempo, reemplazos automáticos por suplentes y una
actualización de presupuesto al final de cada temporada basada en
premios/ajustes de rendimiento.

El objetivo central es entender cómo emerge la concentración de títulos
cuando el desempeño deportivo retroalimenta la capacidad económica para
la temporada siguiente.

---

## Datos

### Datos reales (calibración)
Se utilizan estadísticas reales de jugadores de **La Liga 2024/2025**
(carpeta `PlayerStats`) para construir semillas iniciales de probabilidad
de gol por posición, a partir de variables como:

- `Pos` (posición)
- `MP` (partidos jugados)
- `Gls` (goles)

Con esto se busca reproducir un promedio objetivo de:

- **2.61 goles por partido** (referencia empírica 2024/2025)

### Datos simulados (estado inicial)
- **20 equipos** con presupuesto inicial (en millones de USD).
- **22 jugadores por equipo** (11 titulares + 11 suplentes), con posición
  determinada por dorsal (GK/DF/MF/FW).
- Variables dinámicas: puntos, goles, partidos jugados, títulos,
  lesiones y duración de lesiones.

---

## Metodología

### 1) Estructura de liga
- Formato **round-robin** ida y vuelta (38 jornadas).
- Puntos: 3 (victoria), 1 (empate), 0 (derrota).
- Desempate por diferencia de goles.

### 2) Motor de partidos
Cada partido integra:

- **Diferencia presupuestal**: se calcula un factor para introducir
  asimetrías competitivas (y posibilidad de “batacazos”).
- **Lesiones**: se simulan con Bernoulli por jugador (solo titulares);
  duración aleatoria de 1 a 8 partidos; ingreso automático del suplente.
- **Goles**: se simulan por jugador con una distribución de conteo
  (geométrica), agregando a nivel de equipo para obtener el marcador.

### 3) Actualización económica
Al final de cada temporada:
- Premios decrecientes para el top 10.
- Ajuste adicional por “expectativa vs realidad” según ranking por
  presupuesto.
- Mecanismo acumulativo opcional: si el equipo gana más, recibe más
  presupuesto para el futuro.

### 4) Simulación de múltiples temporadas
Se simulan `n` temporadas en dos modos:

- **Monte Carlo puro** (`mantener_evolucion = FALSE`): cada temporada
  arranca desde el estado inicial.
- **Modo Dinastía** (`mantener_evolucion = TRUE`): el presupuesto se
  acumula temporada a temporada (se reinician puntos, goles y lesiones).

---

## Calibración de goles

Se calibra un parámetro global **θ** que escala las probabilidades de gol
por posición, para que el promedio de goles por partido coincida con el
objetivo (2.61).

Se implementan dos enfoques:

1. **Calibración Monte Carlo iterativa** (ajuste por simulación repetida).
2. **Método de bisección** resolviendo la ecuación:
   \[
   g(\theta) = \mathbb{E}[G(\theta)] - 2.61 = 0
   \]
   donde \(\mathbb{E}[G(\theta)]\) se estima simulando una temporada.

---

## Resultados principales (qué se analiza)

- **Distribución de títulos**: frecuencia de campeonatos por equipo a
  lo largo de muchas temporadas; comparación entre escenarios con y sin
  “equipos grandes” iniciales.
- **Frontera de gasto racional**: curva presupuesto → puntos esperados y
  **costo marginal por punto** (dónde empieza la ineficiencia).
- **Costo promedio por punto (CPP)** y su distribución por segmentos:
  equipos chicos / medianos / grandes, incluyendo ajuste de distribuciones
  (Weibull, Lognormal/Gamma, Normal) para describir el comportamiento del
  sistema.

---

## Estructura recomendada del proyecto

Sugerencia (adaptala a tu repo):
---

## Cómo ejecutar

### Requisitos
- R (>= 4.2 recomendado)
- RStudio (opcional)

### Paquetes
El script instala/carga automáticamente:
`readr`, `dplyr`, `ggplot2`, `tidyr`, `knitr`, `purrr`, `tibble`,
`viridis`, `fitdistrplus`, `patchwork`.

### Render del informe
Desde RStudio:
1. Abrir `SimulacionTemporadaFutbol.Rmd`
2. Click en **Knit** (PDF)

O por consola:
```r
rmarkdown::render("SimulacionTemporadaFutbol.Rmd")
