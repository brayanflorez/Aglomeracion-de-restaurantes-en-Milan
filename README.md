# Réplica del artículo de Leonardi & Moretti (2023)
## Taller 2 — Economía Urbana  
**Autores:** David Flórez • Daniel Hernández  
**Año:** 2024–2025

Este repositorio contiene la réplica completa del Punto 1 del Taller 2 de Economía Urbana, basada en el artículo de Leonardi y Moretti (2023) sobre la localización, crecimiento y estructura espacial de los restaurantes en la ciudad de Milán. El objetivo principal es reproducir las figuras clave del artículo utilizando datos georreferenciados y herramientas modernas de análisis espacial en R.

---

## 📁 Contenido del repositorio

### `R/01_replica_milan.R`
Script principal que:

- Carga y procesa los datos de barrios, población y restaurantes.
- Estandariza claves espaciales y tipos de variable.
- Construye indicadores per cápita de restaurantes en 2004 y 2012.
- Calcula valores relativos respecto al promedio de ciudad.
- Estima tasas de crecimiento logarítmico 2004–2012.
- Define paletas cromáticas y cortes siguiendo el paper original.
- Produce los tres mapas principales:
  - **Mapa 1:** Restaurantes per cápita en 2004.  
  - **Mapa 2:** Restaurantes per cápita en 2012.  
  - **Mapa 3:** Crecimiento per cápita 2004–2012.

Todos los mapas están armonizados tipo “paper”: colores consistentes, leyendas limpias y configuración replicable.

---

### `R/02_densidades_precios.R`
Código que:

- Limpia las variables de precios de los restaurantes en 2004 y 2012.
- Estima densidades no paramétricas usando:
  - Kernel Epanechnikov  
  - Kernel Gaussiano  
- Produce gráficos comparativos entre años y entre kernels.
- Genera densidades para distintos anchos de banda (h/2, h, 2h).
- Exporta figuras listas para informe académico.

Estos gráficos permiten replicar la evidencia de cambios en la distribución de precios.

---

### `R/03_spatial_clustering.R`
Script dedicado al análisis espacial del clustering de restaurantes:

- Identifica los 5 barrios con mayor crecimiento de restaurantes per cápita.
- Crea submuestras espaciales de estos barrios.
- Calcula distancias bilaterales entre restaurantes en 2004 y 2012.
- Estima densidades de distancias observadas.
- Simula posiciones contrafactuales aleatorias dentro de los mismos barrios.
- Construye intervalos simulados (bootstrap) del 5% al 95%.
- Compara distribución observada vs. distribución contrafactual.

El objetivo es evaluar si existe mayor clustering espacial real que el esperado por azar.

---

## 📊 Resultados (carpeta `results/`)
Incluye:

- `figure1_replica.pdf` — Mapa 2004–2012 y crecimiento.  
- `Ejercicio_1_Punto_2_a.pdf` — Densidades por kernel.  
- `Ejercicio_1_Punto_2_.pdf` — Densidades con distintos anchos de banda.  
- `Ejercicio_1_Punto_2_c.pdf` — Clustering espacial vs. simulaciones.

Todos los archivos están en alta resolución para inclusión directa en papers y presentaciones.

---

## 🧰 Paquetes utilizados

- `sf` • análisis espacial  
- `ggplot2` • gráficos de alta calidad  
- `dplyr`, `tidyr` • manipulación de datos  
- `patchwork` • composición de figuras  
- `scales` • manejo de colores y breaks  

---

## 🎯 Objetivo del punto

Replicar fielmente las figuras del paper de Leonardi & Moretti (2023), comprendiendo:

- la distribución espacial de restaurantes,  
- su evolución temporal,  
- cambios en la estructura de precios,  
- y patrones de clustering que reflejan dinámica urbana real.

El repositorio demuestra dominio de análisis espacial, geoprocesamiento, estimación no paramétrica y visualización profesional en R.

---

## 📄 Licencia y uso

Contenido exclusivamente académico para ejercicios del curso de Economía Urbana.  
Reutilización permitida para fines educativos con atribución a los autores.

