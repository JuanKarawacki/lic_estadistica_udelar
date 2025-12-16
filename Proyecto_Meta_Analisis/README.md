# Meta-análisis: Asistencia a clase y rendimiento académico (dataset `dat.crede2010`)

## Descripción
Este proyecto realiza un **meta-análisis** sobre la relación entre la **asistencia a clase** y el
**rendimiento académico** en estudiantes universitarios, utilizando el dataset `dat.crede2010`.
La base reúne **97 coeficientes de correlación** provenientes de **68 estudios**, donde algunos
estudios aportan múltiples tamaños de efecto (múltiples muestras).

## Objetivos
- Estimar la **asociación promedio** entre asistencia y rendimiento académico.
- Evaluar la **heterogeneidad** entre estudios.
- Explorar **moderadores** (p. ej., tipo de criterio de rendimiento y tipo de clase).
- Examinar posibles **sesgos de publicación** y la **robustez** de los hallazgos.

## Datos
- Dataset: `dat.crede2010`
- Efectos: correlaciones (Pearson) entre asistencia y rendimiento.
- Tamaño: 97 tamaños de efecto de 68 estudios.
- Variables disponibles (por muestra):
  - `year`: año de publicación
  - `source`: fuente (revista, tesis, otros)
  - `criterion`: tipo de rendimiento (calificación / GPA)
  - `class`: tipo de asignatura (ciencias / no ciencias)
  - `ni`: tamaño muestral
  - `ri`: correlación observada

> Nota: el dataset proviene de una fuente pública del ecosistema de R (paquetes/datasets para meta-análisis).

## Metodología
- Transformación de tamaños de efecto:
  - correlación de Pearson `r` → **Fisher z** para el análisis
- Modelo principal:
  - **modelo de efectos aleatorios** para estimar el efecto promedio
- Heterogeneidad:
  - evaluación de heterogeneidad total entre estudios (p. ej., estadísticas de heterogeneidad)
- Moderadores:
  - análisis por subgrupos / metarregresión (según corresponda) con variables como:
    - `criterion` (calificación vs GPA)
    - `class` (ciencias vs no ciencias)
- Sesgo de publicación y robustez:
  - análisis exploratorio de sesgo de publicación
  - chequeos de sensibilidad / robustez

## Resultados principales (resumen)
- La asistencia a clase muestra una asociación **positiva y consistente** con el rendimiento académico.
- Se observa **heterogeneidad sustancial** entre estudios, sugiriendo que el tamaño del efecto varía según
  contexto y características de los estudios/cursos.
- Los análisis complementarios (moderadores, sesgo de publicación y robustez) apoyan la conclusión general,
  con matices sobre la magnitud exacta del efecto.

## Cómo reproducir
1. Clonar el repositorio.
2. Abrir el proyecto en RStudio.
3. Instalar dependencias (si aplica).
4. Ejecutar el script principal o compilar el informe:
   - `report/` (R Markdown / Quarto) para generar PDF/HTML
   - `src/` para scripts de limpieza/análisis/modelado

## Estructura del proyecto

/src        # Scripts de preparación, análisis y modelado
/report     # Informe (R Markdown / Quarto) y salidas
/data       # (Opcional) datos auxiliares; el dataset principal se carga desde R
/output     # (Opcional) tablas/figuras exportadas

## Tecnologías
- R
- Paquetes típicos para meta-análisis (p. ej., `metafor`, `meta`, `dmetar`) y manipulación/visualización
  (según el código del proyecto)

## Estado del proyecto
Proyecto académico / informe finalizado (meta-análisis).

## Autor
Juan Karawacki
Licenciatura en Estadística – Universidad de la República

