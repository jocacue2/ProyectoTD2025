## Librerias

```{r}
library(stringr)
library(knitr)
library(ggplot2)
library(tidyverse)
library(pdftools)
library(lubridate)

```

## Importación de datos

```{r}


# ----- Configuración -----
data_dir <- "data/"  # Carpeta con los PDFs
output_dir <- "resultados/"

# Crear carpeta de resultados si no existe
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# ----- Funciones de Extracción -----

extraer_datos_ticket <- function(archivo) {
  # Extraer texto del PDF
  texto <- pdf_text(archivo) %>% paste(collapse = "\n")
  
  # Expresiones regulares para datos clave
  fecha <- str_extract(texto, "\\d{2}/\\d{2}/\\d{4}") %>% dmy()
  total <- str_extract(texto, "TOTAL \\(€\\)\\s+[\\d,]+") %>%
    str_extract("[\\d,]+$") %>% 
    str_replace(",", ".") %>% 
    as.numeric()
  
  # Extraer productos (simplificado)
  lineas_productos <- str_split(texto, "\n")[[1]] %>%
    str_subset("\\d+\\s+[^0-9]+\\s+[\\d,]+") %>%
    str_trim()
  
  productos <- tibble(
    producto = str_extract(lineas_productos, "\\d+\\s+[^0-9]+?(?=\\s+[\\d,])") %>% str_trim(),
    precio = str_extract(lineas_productos, "[\\d,]+$") %>% 
      str_replace(",", ".") %>% 
      as.numeric()
  )
  
  # Extraer IVA
  iva <- str_extract_all(texto, "\\d+%\\s+[\\d,]+\\s+[\\d,]+")[[1]] %>%
    str_split("\\s+") %>%
    map_dfr(~tibble(
      tipo_iva = .x[1],
      base = .x[2] %>% str_replace(",", ".") %>% as.numeric(),
      cuota = .x[3] %>% str_replace(",", ".") %>% as.numeric()
    ))
  
  # Retornar lista con todos los datos
  list(
    fecha = fecha,
    total = total,
    productos = productos,
    iva = iva,
    archivo = basename(archivo))
}

# ----- Procesamiento -----

# Leer todos los PDFs en la carpeta data
archivos <- list.files(data_dir, pattern = "\\.pdf$", full.names = TRUE)

# Procesar todos los tickets
datos <- map(archivos, safely(extraer_datos_ticket)) %>%
  transpose()

# Filtrar tickets procesados correctamente
tickets_ok <- datos$result %>% compact()

# ----- Análisis -----

# 1. Resumen por ticket
resumen_tickets <- map_dfr(tickets_ok, ~tibble(
  fecha = .x$fecha,
  total = .x$total,
  n_productos = nrow(.x$productos),
  tipos_iva = paste(unique(.x$iva$tipo_iva), collapse = ", "),
  archivo = .x$archivo
))

# 2. Productos con precios inconsistentes
productos_comunes <- map_dfr(tickets_ok, ~.x$productos, .id = "ticket_id") %>%
  group_by(producto) %>%
  filter(n() > 1) %>%  # Solo productos que aparecen en múltiples tickets
  mutate(
    diff_precio = (precio - mean(precio)) / mean(precio),
    inconsistente = abs(diff_precio) > 0.1  # >10% de diferencia
  )

# 3. IVA aplicado por categoría
iva_por_tipo <- map_dfr(tickets_ok, ~.x$iva, .id = "ticket_id") %>%
  group_by(tipo_iva) %>%
  summarise(
    n_tickets = n_distinct(ticket_id),
    media_base = mean(base),
    media_cuota = mean(cuota)
)


```
