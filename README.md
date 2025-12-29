# Revisitando las encuestas: Workshop

Este repositorio contiene el material y el código fuente para el sitio web del workshop "Revisitando las encuestas: Una mirada desde las ciencias de la complejidad".

## Estructura

-   `replication-smith2014-table3/`: Scripts base para la replicación del paper.
-   `docs/`: Sitio web generado (HTML). **No editar manualmente**.
-   `*.qmd`: Archivos fuente de los módulos del curso.

## Instalación y Uso

1.  **Clonar:** Clona este repositorio.
2.  **Dependencias:** Abre `revisiting-surveys.Rproj` y ejecuta:
    ```r
    renv::restore()
    ```
3.  **Renderizar Sitio:**
    Desde la terminal:
    ```bash
    quarto render
    ```
    O usando el botón **Render** en RStudio.

## Publicación en GitHub Pages

Este sitio está configurado para publicarse desde la carpeta `/docs` en la rama `main`.

1.  Realiza cambios en los archivos `.qmd`.
2.  Ejecuta `quarto render` para actualizar `/docs`.
3.  Haz commit de los cambios (incluyendo `/docs`) y push a `main`.
4.  En la configuración del repositorio en GitHub -> Pages, selecciona **Source: Deploy from a branch** y elige **main / docs**.
