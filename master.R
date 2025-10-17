# === master.R ===

# Este Script Master ejecuta todos los pasos del trabajo

# Cargar librerias
source(here::here("scripts", "01_load_packages.R"))

# Cargar Bases de datos
source(here::here("scripts", "02_load_data.R"))

# Limpiar Bases de datos y crear columnas necesarias
source(here::here("scripts", "03_clean_data.R"))

# Descripcion basica de la poblacion
source(here::here("scripts", "04_descriptive_data.R"))

# Crear variables para modelo 
source(here::here("scripts", "05_variables.R"))

# Estimar modelos
source(here::here("scripts", "06_model_logit.R"))

# Graficar resultados
source(here::here("scripts", "05_plots.R"))

# Exportar tablas
source(here::here("scripts", "06_tables.R"))

