# Parcial 2 – Simulación de eventos demográficos y proyección de población (USA)

## Descripción
Este proyecto implementa dos componentes principales en Demografía/Modelos poblacionales:

1) **Simulación de trayectorias individuales** para eventos demográficos usando el método de la
transformada inversa bajo un **riesgo constante por intervalos (piecewise-constant hazard)**.
Se simulan dos eventos:
- **Mortalidad (fallecimiento)**: evento universal.
- **Primer hijo (fecundidad condicional)**: evento no universal (puede no ocurrir).

2) **Proyección de población por sexo y edad** mediante el **método de los componentes**,
utilizando tasas específicas de fecundidad por edad y tablas de mortalidad por período
(hombres y mujeres), con comparación final entre pirámide observada vs proyectada.

Caso de estudio: **Estados Unidos**, datos del período con foco en **2015** para la simulación,
y proyección desde el primer año común disponible hasta el último año con datos (comparación
final en 2024).

## Objetivos
- Simular tiempos de ocurrencia de eventos demográficos a partir de tasas por edad usando
  transformada inversa y riesgo constante por intervalos.
- Validar la simulación comparando **Kaplan–Meier** vs supervivencia teórica `S(t)=exp(-H(t))`.
- Proyectar la población por sexo y edad con un esquema tipo Leslie adaptado a insumos anuales.
- Comparar la estructura por edad y sexo proyectada vs observada en el último año disponible.

## Metodología

### Parte 1 — Simulación de eventos demográficos
- Insumos:
  - Tasas de mortalidad por edad `Mx` (HMD).
  - Tasas de fecundidad condicional al primer hijo `m1x` (HFD).
- Procesamiento:
  - Normalización de edades abiertas (“110+” → 110; “55+” → 55; “12-” → 12).
  - Imputación de `0` cuando corresponde riesgo nulo (edades sin nacimientos / NA).
- Simulación:
  - Método de transformada inversa: `T = H^{-1}(-ln(U))`.
  - Implementación numérica con función personalizada `ste()` (archivo `ste.r`).
  - Manejo de eventos no universales asignando `Inf` a no ocurrencias (censura).
- Validación:
  - Comparación visual de curvas:
    - Teórica: `exp(-H(t))`
    - Simulada: Kaplan–Meier sobre 10.000 simulaciones

### Parte 2 — Proyección de población
- Insumos:
  - Población por edad y sexo (HMD, 1-year ages).
  - ASFR (HFD).
  - Tablas de mortalidad por período `Lx` para mujeres y hombres (HMD, 1x1).
- Preparación:
  - Función `data_prep()` para filtrar año inicial común, limpiar edades y reestructurar a formato ancho.
  - Se genera `last_pop.txt` con la estructura observada más reciente para la comparación final.
- Proyección:
  - Proyección anual por sexo usando una matriz tipo Leslie con supervivencia derivada de `Lx`.
  - Nacimientos calculados con ASFR en edades fértiles (12–55).
  - Distribución por sexo usando razón de masculinidad al nacer: `SRB = 1.05`.
- Comparación:
  - Pirámide observada vs proyectada para el último año disponible (2024).

## Datos
Fuentes:
- **Human Mortality Database (HMD)**: mortalidad (`Mx`), life tables (`Lx`), población por edad y sexo.
- **Human Fertility Database (HFD)**: tasas específicas de fecundidad por edad (ASFR) y tasas condicionales al primer hijo (`m1x`).

> Nota: si los archivos originales no se incluyen en el repositorio, ver `data/README.md` para instrucciones de descarga y ubicación.

## Herramientas y tecnologías
- Lenguaje: **R**
- Paquetes principales: `readxl`, `survival`
- Formato de informe: **Quarto (.qmd) → PDF**
- Scripts auxiliares: `ste.r` (función de simulación)

## Estructura del proyecto
/report
  Parcial2.qmd          # informe principal (Quarto)
/src
  ste.r                 # función de simulación (transformada inversa)
/data
  (archivos HMD/HFD)    # si no se suben, dejar instrucciones
/datos_parte_2
  (archivos parte 2)    # insumos para proyección
## Resultados principales (resumen)
- La simulación reproduce adecuadamente las tasas originales:
  - Kaplan–Meier se superpone a la curva teórica en mortalidad y primer hijo.
  - En fecundidad, la supervivencia no cae a 0, reflejando correctamente el evento no universal.
- La proyección por componentes reproduce con alta coherencia la estructura observada (2024),
  especialmente en edades centrales.
- Diferencias esperables en extremos etarios por:
  - ausencia de migración en el modelo,
  - uso de tasas por período sin mejoras futuras,
  - suavización de cohortes inherente al mecanismo Leslie.

## Cómo reproducir
1. Clonar el repositorio.
2. Colocar los archivos de datos en las carpetas correspondientes (`/data` y/o `/datos_parte_2`).
3. Abrir `report/Parcial2.qmd` en RStudio.
4. Ejecutar / renderizar a PDF (Quarto).
5. Verificar que `src/ste.r` esté disponible (se carga con `source("ste.r")`).

## Autoría
David Fernández  
Juan Karawacki  

(Parcial/Trabajo académico)
