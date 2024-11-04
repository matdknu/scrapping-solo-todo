## Cargar la librería stringr
options(scipen = 999)
library(stringr)
library(readr)
library(tidyverse)
data <- read_delim("~/Dropbox/MACI-UDEC/analítica-datos/individual-work/bbdd/productos_celulares.csv", 
              delim = ";", escape_double = FALSE, trim_ws = TRUE) 

library(tidyverse)

# Supongamos que 'data' es el nombre de tu data frame en R
# Dividimos la columna 'Tiendas_y_Precios' en múltiples columnas usando ';' como delimitador
data <- data %>%
  separate(Tiendas_y_Precios, into = paste0("Fragmento", 1:5), sep = ";", fill = "right")

# Creamos una función para extraer la tienda
extraer_tienda <- function(texto) {
  tiendas <- c("PC Factory", "Falabella", "Ripley", "Paris", "AbcDin", "Motorola Shop", 
               "Bodega Oportunidades", "Claro", "Hites", "Mercado Libre", "SP Digital", "Tishop")
  # Buscar la tienda en el texto
  tienda <- tiendas[str_detect(texto, str_c(tiendas, collapse = "|"))]
  if (length(tienda) > 0) return(tienda[1]) else return(NA)
}

# Creamos una función para extraer el precio
extraer_precio <- function(texto) {
  precio <- str_extract(texto, "\\$[0-9.,]+")
  return(precio)
}

# Creamos una función para identificar si es "liberado"
extraer_liberado <- function(texto) {
  if (str_detect(texto, "liberado")) return("Sí") else return("No")
}

# Creamos nuevas columnas para Tienda y Precio para cada fragmento
data <- data %>%
  mutate(
    tienda_1 = str_extract(Fragmento1, "^[^:]+"), # Extraemos la tienda antes de ':'
    precio_1 = str_extract(Fragmento1, "(?<=: )\\$[0-9.,]+"), # Extraemos el precio después de ':'
    tienda_2 = str_extract(Fragmento2, "^[^:]+"),
    precio_2 = str_extract(Fragmento2, "(?<=: )\\$[0-9.,]+"),
    tienda_3 = str_extract(Fragmento3, "^[^:]+"),
    precio_3 = str_extract(Fragmento3, "(?<=: )\\$[0-9.,]+"),
    tienda_4 = str_extract(Fragmento4, "^[^:]+"),
    precio_4 = str_extract(Fragmento4, "(?<=: )\\$[0-9.,]+"),
    tienda_5 = str_extract(Fragmento5, "^[^:]+"),
    precio_5 = str_extract(Fragmento5, "(?<=: )\\$[0-9.,]+")
  ) 


data_long_tienda <- data %>%
  pivot_longer(
    cols = starts_with("tienda_"),
    names_to = "tienda_num",
    values_to = "tienda"
  )

# Convertimos las columnas de precio a formato largo
data_long_precio <- data %>%
  pivot_longer(
    cols = starts_with("precio_"),
    names_to = "precio_num",
    values_to = "precio"
  )

# Unimos ambos dataframes por la posición de las filas
data <- data_long_tienda %>%
  mutate(precio = data_long_precio$precio) %>%
  select(-tienda_num, -precio_1, -precio_2, -precio_3,
         -precio_4, -precio_5) |> drop_na(precio) |> 
  mutate(precio = as.numeric(str_replace_all(precio, "[$.]", ""))) |> 
  mutate(Sistema_operativo = str_replace_all(Sistema_operativo, " ", "_"), 
         OS_categoria = case_when(
           str_detect(Sistema_operativo, "iOS") ~ "iOS",
           str_detect(Sistema_operativo, "Android") ~ "Android",
           str_detect(Sistema_operativo, "MTK") ~ "MTK",
           TRUE ~ "Otros"  # Categoría por defecto si no coincide con ninguno de los anteriores
         ),
         Sistema_operativo = str_replace(Sistema_operativo, "(\\d+)(?!L).*", "\\1"),   # Mantiene solo el número seguido de "L" o sin nada más
         Sistema_operativo = str_remove(Sistema_operativo, "Pie")) |>
  rename(sistema = Sistema_operativo) 


data <- data |> #select(starts_with("fragm")) |> 
  mutate(
    liberado = if_any(starts_with("fragm"), ~ grepl("liberado", ., ignore.case = TRUE)),
    liberado = ifelse(liberado, "sí", "no")  # Convertir TRUE/FALSE a "sí"/"no"
  ) |> 
  select(-Fragmento1, -Fragmento2, -Fragmento3, -Fragmento4, -Fragmento5)





# Crear una nueva columna con solo el número posterior a "Estándar" y antes de "MP"
data$camara <- str_extract(data$Cámara_principal, "(?<=Estándar )[0-9.]+(?= MP)")

data$camara <- as.numeric(data$camara)

# Extraer solo el valor numérico de la columna Batería
data$bateria <- str_extract(data$Batería, "\\d+")

# Convertir la columna a tipo numérico
data$bateria <- as.numeric(data$bateria)


data$memoria <- str_extract(data$Memoria_interna, "\\d+")
data$memoria<- as.numeric(data$memoria)

# Extraer solo el valor numérico Potencia
data$potencia <- str_extract(data$Potencia_de_carga, "\\d+")

# Convertir la columna a tipo numérico
data$potencia <- as.numeric(data$potencia)

data <- data %>%
  rename_with(tolower)

getwd()
library(readr)

write_csv(data, "~/Dropbox/MACI-UDEC/analítica-datos/individual-work/bbdd/data.csv")

