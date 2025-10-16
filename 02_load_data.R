# === 02_load_data.R ===
# Carga y combinación de bases EPH T3 y T4 2024 (individual + hogar)

library(readxl)
library(dplyr)
library(here)

# Leer bases individuales (nombres en MAYÚSCULAS)
ind_t3 <- read_excel(here("data", "usu_individual_T324.xlsx"), sheet = 1) %>%
  rename_with(toupper) %>%
  mutate(trimestre = 3)

ind_t4 <- read_excel(here("data", "usu_individual_T424.xlsx"), sheet = 1) %>%
  rename_with(toupper) %>%
  mutate(trimestre = 4)

# Leer bases de hogar (nombres en MAYÚSCULAS)
hog_t3 <- read_excel(here("data", "usu_hogar_T324.xlsx"), sheet = 1) %>%
  rename_with(toupper) %>%
  mutate(trimestre = 3)

hog_t4 <- read_excel(here("data", "usu_hogar_T424.xlsx"), sheet = 1) %>%
  rename_with(toupper) %>%
  mutate(trimestre = 4)

# Unir individuos con hogares por trimestre (evitar .x/.y usando sufijos)
base_t3 <- left_join(
  ind_t3, hog_t3,
  by = c("CODUSU", "NRO_HOGAR", "trimestre"),
  suffix = c("_ind", "_hog")
)

base_t4 <- left_join(
  ind_t4, hog_t4,
  by = c("CODUSU", "NRO_HOGAR", "trimestre"),
  suffix = c("_ind", "_hog")
)

# Unir todo en una sola base
base_raw <- bind_rows(base_t3, base_t4)

# Verificar estructura
message("✅ Bases cargadas y unidas correctamente.")
message("Observaciones totales: ", nrow(base_raw))
glimpse(base_raw)
