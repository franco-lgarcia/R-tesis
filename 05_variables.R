library(tidyverse)

# Usamos base_filtrada creada
base_vars <- base_filtrada %>%
  mutate(
    ## -------------------- Dependiente --------------------
    # 1 si asiste hoy y el nivel actual es superior (terciario/universitario), 0 si no
    ASISTE_SUPERIOR = as.integer(CH10 == 1 & CH12 %in% c(6, 7)),
    
    ## -------------------- Individuales -------------------
    EDAD  = as.integer(CH06),
    EDAD2 = EDAD^2,
    MUJER = as.integer(CH04 == 2),            # 1 mujer, 0 varón
    
    # Condición laboral del individuo (dicotómica: 1 ocupado, 0 desocupado o inactivo)
    TRABAJA = case_when(
      ESTADO == 1 ~ 1L,                       # ocupado
      ESTADO %in% c(2, 3) ~ 0L,               # desocupado o inactivo
      TRUE ~ NA_integer_
    ),
    
    ## -------------------- Hogar --------------------------
    # Estas vienen de hogar_features (ya calculadas con conv_años y reglas que definimos)
    EDUC_MADRE = as.numeric(EDUC_MADRE),
    EDUC_PADRE = as.numeric(EDUC_PADRE),
    MAX_EDUC_PARENTAL = as.numeric(MAX_EDUC_PARENTAL),
    
    # Jefatura femenina (1 sí, 0 no)
    JEFE_FEMENINO = as.integer(JEFE_FEMENINO),
    
    # Estructura del hogar: 1 biparental, 0 monoparental
    ESTRUCTURA_HOGAR = as.integer(ESTRUCTURA_HOGAR),
    
    # Nº de hermanos convivientes (si es hijo/a del jefe; si no, NA)
    NUM_HERMANOS = ifelse(CH03 == 3 & !is.na(NUM_HIJOS), pmax(NUM_HIJOS - 1L, 0L), NA_integer_),
    
    # Condición laboral del Jefe como DICOTÓMICA: 1 si ocupado, 0 si desocupado/inactivo
    JEFE_OCUPADO = case_when(
      as.character(COND_LAB_JEFE) == "ocupado" ~ 1L,
      as.character(COND_LAB_JEFE) %in% c("desocupado", "inactivo") ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    # Concentración ingreso del jefe (0–1; ya calculada)
    CONC_ING_JEFE = as.numeric(CONC_ING_JEFE)
  )

## -------------------- Controles --------------------------
# LOG_ING_PC: preferimos el IPCF del módulo hogar si está; si no, lo derivamos como ING_HOGAR/IX_TOT
base_vars <- base_vars %>%
  mutate(
    IPCF_BASE = dplyr::coalesce(as.numeric(.data[["IPCF_hog"]]),  # si está, usalo
                                as.numeric(.data[["IPCF_ind"]])), # (backup poco ideal)
    ING_PC_ALT = ifelse(!is.na(ING_HOGAR) & !is.na(IX_TOT) & IX_TOT > 0,
                        as.numeric(ING_HOGAR) / as.numeric(IX_TOT), NA_real_),
    ING_PC = dplyr::coalesce(IPCF_BASE, ING_PC_ALT),
    LOG_ING_PC = ifelse(!is.na(ING_PC) & ING_PC > 0, log(ING_PC), NA_real_)
  )

# REGION: factor ordenado con etiquetas; usa REGION_hog si existe, si no REGION_ind
base_vars <- base_vars %>%
  mutate(
    # Usamos módulo hogar si está; si no, el individual
    region_code = dplyr::coalesce(as.numeric(REGION_hog), as.numeric(REGION_ind)),
    
    # Factor con etiquetas, manteniendo CODIGOS ORIGINALES
    REGION = factor(dplyr::case_when(
      region_code == 1  ~ "GBA",
      region_code == 40 ~ "Pampeana",
      region_code == 41 ~ "NOA",
      region_code == 42 ~ "NEA",
      region_code == 43 ~ "Cuyo",
      region_code == 44 ~ "Patagonia",
      TRUE ~ NA_character_
    ), levels = c("GBA","Pampeana","NOA","NEA","Cuyo","Patagonia"))
  )

## -------------------- Dataset final para el modelo --------------------------
base_modelo <- base_vars %>%
  transmute(
    # y
    ASISTE_SUPERIOR,
    
    # individuales
    EDAD, EDAD2, MUJER, TRABAJA,
    
    # hogar
    EDUC_MADRE, EDUC_PADRE, MAX_EDUC_PARENTAL,
    JEFE_OCUPADO, JEFE_FEMENINO, NUM_HERMANOS, ESTRUCTURA_HOGAR,
    CONC_ING_JEFE,
    
    # controles
    LOG_ING_PC, REGION
  )

# Chequeos rápidos

glimpse(base_modelo)
summary(base_modelo)

n_total    <- nrow(base_modelo)
n_complete <- sum(complete.cases(base_modelo))   # o nrow(na.omit(base_modelo))
pct_complete <- 100 * n_complete / n_total

cat("Observaciones totales:", n_total, "\n")
cat("Observaciones completas:", n_complete, "\n")
cat("Porcentaje completas:", round(pct_complete, 2), "%\n")
