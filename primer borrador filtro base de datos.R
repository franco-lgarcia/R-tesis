# Librerías necesarias
library(readxl)
library(dplyr)

# Cargar los datos (ajustá las rutas si hace falta)
eph_t3 <- read_excel("C:/Users/franc/OneDrive/Escritorio/Economia/UNR/Tesis - Econometria 2/Base de datos/EPH_usu_3_Trim_2024_xls/EPH_usu_3er_Trim_2024_xlsx/usu_individual_T324.xlsx")
eph_t4 <- read_excel("C:/Users/franc/OneDrive/Escritorio/Economia/UNR/Tesis - Econometria 2/Base de datos/EPH_usu_4_Trim_2024_xls/EPH_usu_4to_Trim_2024_xlsx/usu_individual_T424.xlsx", sheet = "usu_individual_T424")

# Combinar los dos trimestres primero
eph_combinado <- bind_rows(eph_t3, eph_t4)

# Eliminar duplicados por identificadores
eph_combinado <- eph_combinado %>%
  distinct(CODUSU, NRO_HOGAR, COMPONENTE, .keep_all = TRUE)

# Filtrar por edad, parentesco e instrucción
eph_filtrado <- eph_combinado %>%
  filter(
    CH06 >= 17, CH06 <= 21,       # Edad entre 17 y 21
    CH03 == 3,                    # Hijo/a
    NIVEL_ED %in% c(4, 5)                 # Universitario o Terciario Incompleto
  )

# Mostrar cantidad de observaciones finales
cat("Cantidad de observaciones después de unir y filtrar:", nrow(eph_filtrado), "\n")

# Mostrar resumen de asistencia a educación
# Contar asistencia con etiquetas legibles
eph_filtrado %>%
  count(CH10) %>%
  mutate(
    CH10 = case_when(
      CH10 == 1 ~ "Sí, asiste",
      CH10 == 2 ~ "No asiste, pero asistió",
      CH10 == 3 ~ "Nunca asistió",
      TRUE ~ "Otro / Ns-Nr"
    )
  )
# Ver columnas disponibles (opcional)
# View(eph_filtrado)
# glimpse(eph_filtrado)