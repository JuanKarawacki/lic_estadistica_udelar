# Trabajo final – Introducción a los modelos multinivel

## Descripción
Trabajo final del curso *Introducción a los Modelos Multinivel*. El estudio analiza los niveles de
satisfacción de los usuarios del transporte interurbano en función de características personales
de los pasajeros y condiciones operativas de las líneas de transporte. La estructura jerárquica
de los datos —pasajeros agrupados dentro de líneas— motiva el uso de modelos multinivel.

## Objetivos
- Analizar los determinantes de la satisfacción de los usuarios del transporte interurbano.
- Evaluar la presencia de variabilidad entre líneas de transporte.
- Comparar el enfoque de modelos lineales clásicos con modelos multinivel.
- Estimar la proporción de variabilidad atribuible a diferencias entre líneas mediante el ICC.

## Metodología
- Análisis descriptivo inicial de las variables de interés.
- Ajuste de modelos lineales clásicos y diagnóstico de supuestos.
- Ajuste de modelos de efectos mixtos con intercepto aleatorio por línea de transporte.
- Estimación e interpretación del coeficiente de correlación intraclase (ICC).
- Evaluación de la significancia estadística de variables individuales.

## Datos
- Observaciones de usuarios del transporte interurbano.
- Estructura jerárquica: pasajeros anidados en 17 líneas de transporte.
- Variable respuesta: nivel de satisfacción del usuario.
- Variables explicativas: género, edad, frecuencia de uso y puntualidad del servicio.
- Los datos no se incluyen en el repositorio por razones académicas.

## Herramientas y tecnologías
- Lenguaje: R
- Paquetes principales: `lme4`, `lmerTest`, `tidyverse`
- Entorno de trabajo: RStudio
- Formato de informe: R Markdown / PDF

## Estructura del proyecto
/src        # Scripts de análisis y ajuste de modelos
/report     # Informe final del trabajo
/data       # Datos (no incluidos)

## Resultados principales
- Se identifican diferencias significativas en la satisfacción promedio entre líneas de transporte.
- El coeficiente de correlación intraclase (ICC) es cercano a 0.40, indicando que una proporción
  sustancial de la variabilidad se explica por diferencias entre líneas.
- Las variables género, edad, frecuencia de uso y puntualidad del servicio presentan efectos
  estadísticamente significativos sobre la satisfacción.
- Los modelos multinivel muestran un mejor ajuste que los modelos lineales clásicos.

## Cómo reproducir el análisis
1. Clonar el repositorio.
2. Abrir el proyecto en RStudio.
3. Ejecutar los scripts contenidos en la carpeta `/src`.
4. Compilar el informe ubicado en `/report`.

## Estado del proyecto
Proyecto académico finalizado.

## Autores
Diego Da Rosa  
Lucas Giúdice  
Juan Karawacki  
Bruno Pintos  

Licenciatura en Estadística – Universidad de la República
