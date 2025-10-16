# === 03_clean_data.R ===
library(dplyr)

# --- Filtro base ---
base_filtrada <- base_raw %>%
  filter(
    CH03 == 3,                   # Hijo/a
    CH06 >= 17 & CH06 <= 23,     # Edad 17 - 23 años
    CH07 == 5,                   # Soltero
    NIVEL_ED %in% c(4, 5)        # Secundario completo / Universitario incompleto
  )

message("✅ Filtro inicial aplicado.")
message("Observaciones luego del filtro: ", nrow(base_filtrada))

# ---- Función necesaria para conversión NIVEL (CH12) + AÑO APROBADO (CH14) -> años totales ----
conv_años <- function(nivel, año, finalizo = NA_integer_) {
  # Normaliza CH14 y trata 98/99 como NA
  año <- suppressWarnings(as.numeric(año))
  año[año %in% c(98, 99)] <- NA_real_
  año <- pmax(año, 0, na.rm = FALSE)  # evita negativos si los hubiera
  
  # Años base por nivel
  educ_base <- case_when(
    nivel == 1 ~ 0,    # Jardín/Preescolar
    nivel == 2 ~ 6,    # Primario
    nivel == 3 ~ 9,    # EGB
    nivel == 4 ~ 12,   # Secundario
    nivel == 5 ~ 12,   # Polimodal
    nivel == 6 ~ 12,   # Terciario: se sumarán hasta 3 extra
    nivel == 7 ~ 12,   # Universitario: se sumarán hasta 5 extra
    nivel == 8 ~ 17,   # Posgrado: 17 (=12+5) + hasta 2 extra
    TRUE       ~ NA_real_
  )
  
  # Años adicionales por CH14 con topes por nivel
  educ_extra <- case_when(
    nivel == 6 ~ pmin(año, 3, na.rm = FALSE),  # Terciario
    nivel == 7 ~ pmin(año, 5, na.rm = FALSE),  # Universitario
    nivel == 8 ~ pmin(año, 2, na.rm = FALSE),  # Posgrado (2 años máx)
    TRUE       ~ 0
  )
  
  # Ciclo completo para fallback cuando finalizó y falta CH14
  ciclo_completo <- case_when(
    nivel == 1 ~ 0,
    nivel == 2 ~ 6,
    nivel == 3 ~ 9,
    nivel %in% c(4, 5) ~ 12,
    nivel == 6 ~ 15,   # 12 + 3
    nivel == 7 ~ 17,   # 12 + 5
    nivel == 8 ~ 19,   # 17 + 2
    TRUE ~ NA_real_
  )
  
  # nivel anterior para INCOMPLETOS (cuando no hay CH14)
  nivel_anterior <- dplyr::case_when(
    nivel %in% c(1,2,3) ~ 0,  # sin anterior
    nivel == 4 ~ 6,        # anterior a secundario: primario completo
    nivel == 5 ~ 9,        # anterior a polimodal: EGB completo
    nivel == 6 ~ 12,       # anterior a terciario: secundario completo
    nivel == 7 ~ 12,       # anterior a universitario: secundario completo
    nivel == 8 ~ 17,       # anterior a posgrado: universitario completo
    TRUE ~ NA_real_
  )
  
  # Suma base + extra
  educ_total <- educ_base + educ_extra
  
  # Respuestas:
  res <- dplyr::case_when(
    # INCOMPLETO en superior (6,7,8) con CH14 disponible -> usar progreso
    !is.na(nivel) & finalizo != 1 & nivel >= 6 & !is.na(año) ~ educ_total,
    
    # INCOMPLETO sin CH14 (o en niveles básicos) -> nivel anterior
    !is.na(nivel) & finalizo != 1 ~ nivel_anterior,
    
    # COMPLETO y falta CH14 -> ciclo completo (fallback)
    !is.na(nivel) & finalizo == 1 & is.na(año) ~ ciclo_completo,
    
    # Resto: usar educ_total (incluye completo con CH14 en superior, y básicos)
    TRUE ~ educ_total
  )
  
  res
}

# --- Variables de hogar ---
hogar_features <- base_raw %>%
  group_by(CODUSU, NRO_HOGAR, trimestre) %>%
  summarise(
    # 1) Jefatura femenina
    JEFE_FEMENINO = {
      sx_jefe <- CH04[which(CH03 == 1)[1]]
      ifelse(length(sx_jefe) == 0 || is.na(sx_jefe), NA_integer_,
             ifelse(sx_jefe == 2, 1L, 0L))
    },
    
    # 2) Estructura del hogar
    ESTRUCTURA_HOGAR = {
      tiene_jefe    <- any(CH03 == 1, na.rm = TRUE)
      tiene_conyuge <- any(CH03 == 2, na.rm = TRUE)
      as.integer(tiene_jefe & tiene_conyuge)
    },
    
    # 3) Hijos del jefe
    NUM_HIJOS = sum(CH03 == 3, na.rm = TRUE),
    
    # 4) Educación madre/padre
    EDUC_MADRE = {
      idx_madre <- which((CH03 %in% c(1, 2)) & CH04 == 2)[1]
      if (length(idx_madre) == 0 || is.na(idx_madre)) NA_integer_
      else conv_años(CH12[idx_madre], CH14[idx_madre], CH13[idx_madre])[1]
    },
    EDUC_PADRE = {
      idx_padre <- which((CH03 %in% c(1, 2)) & CH04 == 1)[1]
      if (length(idx_padre) == 0 || is.na(idx_padre)) NA_integer_
      else conv_años(CH12[idx_padre], CH14[idx_padre], CH13[idx_padre])[1]
    },
    
    # 5) Condición laboral del jefe
    COND_LAB_JEFE = {
      est_jefe <- ESTADO[which(CH03 == 1)[1]]
      case_when(
        est_jefe == 1 ~ "ocupado",
        est_jefe == 2 ~ "desocupado",
        est_jefe == 3 ~ "inactivo",
        TRUE ~ NA_character_
      )
    },
    
    # 6) Ingreso del jefe (primer no-NA y no-negativo)
    ING_JEFE = {
      v <- as.numeric(P47T[CH03 == 1])
      v <- v[!is.na(v) & v >= 0]
      if (length(v)) v[1] else NA_real_
    },
    
    # 7) Ingreso del hogar (permitimos ceros)
    ING_HOGAR = {
      ih <- as.numeric(ITF_hog)
      ih <- ih[!is.na(ih)]         
      if (length(ih)) ih[1] else NA_real_
    },
    
    # 8) Proporción jefe/hogar (NA si ih <= 0)
    CONC_ING_JEFE = {
      ij <- as.numeric(P47T[CH03 == 1]); ij <- ij[!is.na(ij) & ij >= 0]; ij <- if (length(ij)) ij[1] else NA_real_
      ih <- as.numeric(ITF_hog);         ih <- ih[!is.na(ih)];           ih <- if (length(ih)) ih[1] else NA_real_
      if (is.na(ij) || is.na(ih) || ih <= 0) NA_real_ else pmin(ij/ih, 1)
    },
    
    .groups = "drop"
  ) %>%
  mutate(
    MAX_EDUC_PARENTAL = pmax(EDUC_MADRE, EDUC_PADRE, na.rm = TRUE),
    COND_LAB_JEFE = factor(COND_LAB_JEFE, levels = c("ocupado","desocupado","inactivo"))
  )

# --- Asegurar tipos antes de unir ---
base_filtrada <- base_filtrada %>%
  mutate(CODUSU = as.character(CODUSU),
         NRO_HOGAR = as.integer(NRO_HOGAR),
         trimestre = as.integer(trimestre))

hogar_features <- hogar_features %>%
  mutate(CODUSU = as.character(CODUSU),
         NRO_HOGAR = as.integer(NRO_HOGAR),
         trimestre = as.integer(trimestre))

# --- Unión hogar -> jóvenes ---
base_filtrada <- left_join(base_filtrada, hogar_features,
                           by = c("CODUSU","NRO_HOGAR","trimestre"))

message("✅ Unión completada. Obs: ", nrow(base_filtrada))

# --- Asistencia a educación superior ---
asisten_superior <- base_filtrada %>%
  filter(CH10 == 1, CH12 %in% c(6, 7, 8))

no_asisten_superior <- base_filtrada %>%
  filter(!(CH10 == 1 & CH12 %in% c(6, 7, 8)))

# --- Totales ---
n_total <- nrow(base_filtrada)
n_asisten <- nrow(asisten_superior)
n_no_asisten <- nrow(no_asisten_superior)
porcentaje_asisten <- round((n_asisten / n_total) * 100, 1)

# --- Resultados ---
cat("🎓 Total de individuos en la muestra:", n_total, "\n")
cat("✅ Asisten a educación superior:", n_asisten, "\n")
cat("🚫 No asisten a educación superior:", n_no_asisten, "\n")
cat("📊 Porcentaje que asiste:", porcentaje_asisten, "%\n")



