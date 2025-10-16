# === Variables de hogar (resumen por hogar) ===
library(dplyr)

hogar_features <- base_raw %>%
  group_by(CODUSU, NRO_HOGAR, trimestre) %>%
  summarise(
    # 1) Jefatura femenina
    JEFE_FEMENINO = {
      sx_jefe <- CH04[which(CH03 == 1)[1]]
      ifelse(length(sx_jefe) == 0 || is.na(sx_jefe), NA_integer_, ifelse(sx_jefe == 2, 1L, 0L))
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
      if (length(idx_madre) == 0 || is.na(idx_madre)) NA_integer_ else
        conv_años(CH12[idx_madre], CH14[idx_madre], CH13[idx_madre])[1]
    },
    EDUC_PADRE = {
      idx_padre <- which((CH03 %in% c(1, 2)) & CH04 == 1)[1]
      if (length(idx_padre) == 0 || is.na(idx_padre)) NA_integer_ else
        conv_años(CH12[idx_padre], CH14[idx_padre], CH13[idx_padre])[1]
    },
    
    # 5) Condición laboral del jefe
    COND_LAB_JEFE = {
      est_jefe <- ESTADO[which(CH03 == 1)[1]]
      dplyr::case_when(
        est_jefe == 1 ~ "ocupado",
        est_jefe == 2 ~ "desocupado",
        est_jefe == 3 ~ "inactivo",
        TRUE ~ NA_character_
      )
    },
    
    # 6) Ingreso del jefe (primer no-NA y no-negativo del jefe)
    ING_JEFE = {
      v <- as.numeric(P47T[CH03 == 1])
      v <- v[!is.na(v) & v >= 0]
      if (length(v)) v[1] else NA_real_
    },
    
    # 7) Ingreso del hogar (primer no-NA y >0 dentro del grupo)
    ING_HOGAR = {
      ih <- as.numeric(ITF_hog)
      ih <- ih[!is.na(ih) & ih > 0]
      if (length(ih)) ih[1] else NA_real_
    },
    
    # 8) Proporción ingreso jefe / hogar (truncada en 1)
    CONC_ING_JEFE = {
      # volvemos a calcular localmente para usar los mismos criterios de limpieza
      ij <- as.numeric(P47T[CH03 == 1]); ij <- ij[!is.na(ij) & ij >= 0]; ij <- if (length(ij)) ij[1] else NA_real_
      ih <- as.numeric(ITF_hog);         ih <- ih[!is.na(ih) & ih > 0];  ih <- if (length(ih)) ih[1] else NA_real_
      if (is.na(ij) || is.na(ih)) NA_real_ else pmin(ij/ih, 1)
    },
    
    .groups = "drop"
  ) %>%
  mutate(
    MAX_EDUC_PARENTAL = pmax(EDUC_MADRE, EDUC_PADRE, na.rm = TRUE),
    COND_LAB_JEFE = factor(COND_LAB_JEFE, levels = c("ocupado","desocupado","inactivo"))
  )


# ========= UNION HOGAR -> JOVENES ==================
base_filtrada <- base_filtrada %>%
  left_join(hogar_features, by = c("CODUSU","NRO_HOGAR","trimestre"))
  
 message("✅ Unión completada. Obs: ", nrow(base_filtrada))

stopifnot(nrow(base_filtrada) == nrow(base_filtrada %>% distinct(CODUSU,NRO_HOGAR,trimestre, COMPONENTE, .keep_all = TRUE)))

stopifnot("COMPONENTE" %in% names(base_filtrada))

# Contar hogares únicos en cada base
n_hog_filtrada <- n_distinct(base_filtrada %>% select(CODUSU, NRO_HOGAR, trimestre))
n_hog_features <- n_distinct(hogar_features %>% select(CODUSU, NRO_HOGAR, trimestre))

cat("Hogares únicos en base_filtrada:", n_hog_filtrada, "\n")
cat("Hogares únicos en hogar_features:", n_hog_features, "\n")

names(base_filtrada)[names(base_filtrada) %in% c(
  "JEFE_FEMENINO", "ESTRUCTURA_HOGAR", "EDUC_MADRE", "EDUC_PADRE",
  "MAX_EDUC_PARENTAL", "COND_LAB_JEFE", "ING_JEFE", "ING_HOGAR", "CONC_ING_JEFE"
)]

summary(select(base_filtrada,
               JEFE_FEMENINO, ESTRUCTURA_HOGAR, EDUC_MADRE, EDUC_PADRE,
               MAX_EDUC_PARENTAL, COND_LAB_JEFE, ING_JEFE, ING_HOGAR, CONC_ING_JEFE))

names(hogar_features)

str(select(base_filtrada, CODUSU, NRO_HOGAR, trimestre))
str(select(hogar_features, CODUSU, NRO_HOGAR, trimestre))


tmp_match <- inner_join(
  base_filtrada %>% distinct(CODUSU, NRO_HOGAR, trimestre),
  hogar_features %>% distinct(CODUSU, NRO_HOGAR, trimestre),
  by = c("CODUSU","NRO_HOGAR","trimestre")
)

nrow(tmp_match); n_distinct(tmp_match$CODUSU)


