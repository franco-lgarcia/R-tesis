library(dplyr)
library(tidyr)

# 1) Recorte del universo muestral
base_u <- base_raw %>%
  filter(
    CH06 >= 18 & CH06 <= 23,      # Edad entre 18 y 22 años
    CH07 == 5,                    # Solteros
    NIVEL_ED %in% c(4, 5)         # Secundario completo o universitario incompleto
  )

# 2) Indicador de "convivencia con padres" según definición de tesina
#    (hijo/a del/de la jefe/a de hogar en la vivienda)
base_u <- base_u %>%
  mutate(hijo_conviviente = if_else(CH03 == 3, 1L, 0L))  # 3 = hijo/a (ajusta si difiere)

# 3) Total convivientes vs no convivientes (conteo y %)
tabla_total <- base_u %>%
  summarise(
    n_total = n(),
    n_conviv = sum(hijo_conviviente == 1, na.rm = TRUE),
    n_no_conviv = sum(hijo_conviviente == 0, na.rm = TRUE),
    p_conviv = n_conviv / n_total,
    p_no_conviv = n_no_conviv / n_total
  )

# 4) Desagregado por edad
tabla_edad <- base_u %>%
  group_by(CH06) %>%
  summarise(
    n = n(),
    n_conviv = sum(hijo_conviviente == 1, na.rm = TRUE),
    p_conviv = n_conviv / n
  ) %>%
  ungroup()

# 5) (Opcional) Desagregado por asistencia Y
base_u <- base_u %>%
  mutate(
    ASISTE_SUPERIOR = case_when(
      CH10 == 1 & CH12 %in% c(6,7,8) ~ 1L,                     # asiste y es superior
      CH10 == 1 & CH12 %in% c(1,2,3,4,5,9) ~ 0L,               # asiste pero no es superior
      CH10 %in% c(2,0) ~ 0L,                                   # no asiste (ajusta si 0 no existe)
      CH10 %in% c(9) | CH12 %in% c(99) ~ NA_integer_,          # Ns/Nr -> NA
      TRUE ~ NA_integer_
    )
  )

tabla_asistencia <- base_u %>%
  group_by(ASISTE_SUPERIOR) %>%
  summarise(
    n = n(),
    n_conviv = sum(hijo_conviviente == 1, na.rm = TRUE),
    p_conviv = n_conviv / n
  ) %>%
  ungroup()

show(tabla_asistencia)

# ¿La brecha de 2,6 p.p. es significativa?
prop.test(x = c(2172, 1492), n = c(2818, 1872))   # conviven entre Y=1 vs Y=0

show(tabla_edad)

# 6) (Opcional) Por región
tabla_region <- base_u %>%
  group_by(REGION_ind) %>%
  summarise(
    n = n(),
    n_conviv = sum(hijo_conviviente == 1, na.rm = TRUE),
    p_conviv = n_conviv / n
  ) %>% ungroup()

show(tabla_region)
