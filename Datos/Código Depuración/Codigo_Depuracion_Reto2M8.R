# Librerias

if(!require(dplyr)){
  install.packages('dplyr')
  library(dplyr)
}

if(!require(readr)){
  install.packages('readr')
  library(readr)
}



# Cargamos los datos
data_path <- 'Datos/Base de datos original/datos.csv'
data <- read_csv(data_path)

# Mostrar las primeras filas del DataFrame para verificar la correcta carga de los datos
head(data)

# 1. Depuramos los datos

# Seleccionamos solo las columnas necesarias

variables_interes <- c("cntry", "agea", "gndr", "eisced", "hinctnta", "evmar", "health", "happy")
datos_filtrados <- data %>%
  select(all_of(variables_interes))

# Filtramos las observaciones no válidas (los numeros que aparecen se corresponden a valores nulos o del tipo 'No sabe / No contesta')
datos_depurados <- datos_filtrados %>%
  filter(!is.na(cntry),
         agea != 999,
         !hinctnta %in% c(77, 88, 99),
         !health %in% c(7, 8, 9),
         !evmar %in% c(7, 9),
         !happy %in% c(77, 88, 99),
         !eisced %in% c(55, 77, 88, 99))


# Convertimos las columnas categóricas a factores
datos_depurados <- datos_depurados %>%
  mutate(
    cntry = as.factor(cntry),
    gndr = as.factor(gndr),
    eisced = as.factor(eisced),
    evmar = as.factor(evmar),
    health = as.factor(health),
    happy = as.factor(happy)
  )

summary(datos_depurados)

# Guardar los datos depurados en un nuevo archivo CSV
write_csv(datos_depurados, 'Datos/Base de datos depurada/datos_clean.csv')
