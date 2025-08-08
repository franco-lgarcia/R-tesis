library(dplyr)

# Paso 1: personas que actualmente asisten a algún establecimiento educativo
asisten <- base_raw %>%
  filter(CH10 == 1)

# Paso 2: de esos, quedarnos con quienes están cursando universidad
asisten_uni <- asisten %>%
  filter(CH12 %in% c(7))

# Paso 3: de esos, cuántos tienen entre 18 y 22 años
asisten_uni_jovenes <- asisten_uni %>%
  filter(CH06 >= 18 & CH06 <= 22)

# Paso 4: cálculo del porcentaje
total_asisten_uni <- nrow(asisten_uni)
jovenes_18_22 <- nrow(asisten_uni_jovenes)
porcentaje_jovenes <- (jovenes_18_22 / total_asisten_uni) * 100

# Resultado
cat("🎓 Total que asisten actualmente a universidad:", total_asisten_uni, "\n")
cat("🧒 De 18 a 22 años:", jovenes_18_22, "\n")
cat("📊 Porcentaje:", round(porcentaje_jovenes, 2), "%\n")
