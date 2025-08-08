# === 03_clean_data.R ===
# Limpieza y filtrado de la base

# Filtrado y limpieza de base_raw para población objetivo

library(dplyr)

# 1. --- Filtro básico ---
base_filtrada <- base_raw %>%
  filter(
    CH06 >= 18 & CH06 <= 22,      # Edad entre 18 y 22 años
    CH07 == 5,                    # Solteros
    CH03 == 3,                    # Hijo/a del jefe
    NIVEL_ED %in% c(4, 5)         # Secundario completo o universitario incompleto
  )

message("✅ Filtro inicial aplicado.")
message("Observaciones luego del filtro: ", nrow(base_filtrada))

# 2. --- Eliminar observaciones con NA en variables clave ---
vars_clave <- c("CH06","CH07", "CH03", "NIVEL_ED", "CH04", "CH10", "CH12", "CH13")

base_sin_na <- base_filtrada %>%
  filter(across(all_of(vars_clave), ~ !is.na(.)))

message("🧹 Eliminadas por NA en variables clave: ", nrow(base_filtrada) - nrow(base_sin_na))

# 3. --- Eliminar observaciones con edad-nivel educativo incoherente ---
base_coherente <- base_sin_na %>%
  filter(!(CH06 == 18 & NIVEL_ED >= 6))  # por ejemplo: 18 años y posgrado

message("🧠 Eliminadas por incoherencia edad-nivel: ", nrow(base_sin_na) - nrow(base_coherente))

# 4. --- Eliminar columnas duplicadas (.y) que no se usan ---
base_final <- base_coherente %>%
  select(-ends_with(".y"))

# 5. --- Guardar base limpia con todas las variables ---
message("✅ Limpieza terminada. Variables conservadas: ", ncol(base_final))
message("Observaciones finales: ", nrow(base_final))
glimpse(base_final)

# Asisten actualmente a la universidad o terciario
asisten_superior <- base_final %>%
  filter(CH10 == 1, CH12 %in% c(6, 7))

# No asisten actualmente (pueden haber asistido o no)
no_asisten_superior <- base_final %>%
  filter(!(CH10 == 1 & CH12 %in% c(6, 7)))

# Totales
n_total <- nrow(base_final)
n_asisten <- nrow(asisten_superior)
n_no_asisten <- nrow(no_asisten_superior)
porcentaje_asisten <- round((n_asisten / n_total) * 100, 1)

# Resultados
cat("🎓 Total de individuos en la muestra:", n_total, "\n")
cat("✅ Asisten a educación superior:", n_asisten, "\n")
cat("🚫 No asisten a educación superior:", n_no_asisten, "\n")
cat("📊 Porcentaje que asiste:", porcentaje_asisten, "%\n")


