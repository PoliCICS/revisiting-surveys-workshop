# Revisitando las encuestas: Workshop

**Una mirada desde las ciencias de la complejidad**

> **Sitio Web del Curso:** [https://policics.github.io/revisiting-surveys-workshop/0-index.html](https://policics.github.io/revisiting-surveys-workshop/0-index.html)

Este repositorio contiene los materiales, código y datos para el workshop **"Revisitando las encuestas"**. Este curso ofrece una introducción práctica a metodologías avanzadas para el análisis de encuestas, moviéndose desde la asunción de independencia de observaciones hacia una perspectiva interdependiente y estructural.

## Objetivos del Taller

1.  **Deconstruir la Encuesta:** Entender las limitaciones del paradigma tradicional atomista.
2.  **Redes Latentes:** Aprender a imputar estructuras sociales invisibles (homofilia) dentro de datos de encuestas tradicionales (metodología Smith & McPherson).
3.  **Psicometría de Redes:** Introducir modelos de sistemas complejos (Ising Models) para analizar sistemas de creencias y actitudes.

## Módulos

El taller se divide en 5 secciones prácticas:

1.  **Introducción:** La encuesta como sistema complejo.
2.  **La Fuerza Homofílica:** Replicación de parámetros de segregación social usando la GSS.
3.  **Imputación de Contexto:** Reconstrucción de la red social latente en la encuesta ANES.
4.  **Ising & Psicometría:** Modelos de redes para actitudes políticas.
5.  **Conclusiones:** Desafíos prácticos y extensiones con datos globales (ELSOC, SHARE, etc.).

## Estructura del Repositorio

-   `replication-smith2014-table3/`: Scripts base para la replicación del paper de Smith & McPherson (2014).
-   `scripts/`: Scripts de análisis y plantillas para estudiantes.
-   `docs/`: Sitio web generado (HTML).
-   `*.qmd`: Archivos fuente de los módulos del curso (Quarto).
-   `data/`: (No incluido en repo por tamaño/licencia, ver instrucciones de descarga en el sitio).

## Reproducibilidad

Este proyecto utiliza `renv` para gestionar las dependencias de R y asegurar la reproducibilidad.

1.  Clona este repositorio.
2.  Abre el proyecto en RStudio (`revisiting-surveys.Rproj`).
3.  Restaura el entorno:
    ```r
    renv::restore()
    ```
4.  Renderiza el sitio web:
    ```bash
    quarto render
    ```

## Docentes

-   **Aníbal Olivera**
-   **Jorge Fábrega**
