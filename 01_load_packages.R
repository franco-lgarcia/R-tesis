# === 01_load_packages.R ===
# Carga de paquetes necesarios

# Instala pacman si no está presente
if (!require("pacman")) install.packages("pacman")

# Cargar o instalar paquetes
pacman::p_load(
  # Utilidades generales
  here,           # rutas dinámicas
  tidyverse,      # dplyr, ggplot2, tidyr, readr, etc.
  haven,          # leer archivos .dta (Stata) si hace falta
  janitor,        # limpiar nombres de variables, tabulados
  skimr,          # resumenes estadísticos rápidos
  naniar,         # explorar datos faltantes
  readxl,         # leer archivos en formato xls
  
  # Análisis y modelos
  survey,         # manejo de ponderadores (como los de EPH)
  glmnet,         # regresión Ridge y Lasso
  fixest,         # regresión logit robusta y eficiente
  margins,        # efectos marginales para logit/probit
  broom,          # pasar modelos a tidy dataframes
  
  # Visualización
  ggplot2,        # gráficos personalizables
  esquisse,       # gráfico interactivo (opcional)
  
  # Presentación de resultados
  gtsummary,      # tablas tipo paper
  stargazer,      # exportar regresiones a LaTeX o HTML
  modelsummary    # alternativa moderna a stargazer
)

message("✅ Paquetes cargados correctamente.")