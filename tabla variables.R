# === Cuadro descriptivo: TOTAL / ASISTE / NO ASISTE ===
library(dplyr)
library(tidyr)
library(kableExtra)

# 1) Datos y variable de grupo
df <- base_modelo %>%
  filter(!is.na(ASISTE_SUPERIOR)) %>%
  mutate(grupo = factor(ASISTE_SUPERIOR, levels = c(1,0),
                        labels = c("Asiste","No asiste")))

# Variables a mostrar (orden)
vars <- c(
  "EDAD","EDAD2","LOG_ING_PC",
  "EDUC_MADRE","EDUC_PADRE","MAX_EDUC_PARENTAL",
  "MUJER","TRABAJA","JEFE_OCUPADO","JEFE_FEMENINO",
  "NUM_HERMANOS","ESTRUCTURA_HOGAR","CONC_ING_JEFE"
)

# Etiquetas en español para la primera columna
var_labels <- c(
  EDAD="Edad", EDAD2="Edad²", LOG_ING_PC="Log ingreso per cápita",
  EDUC_MADRE="Educación madre", EDUC_PADRE="Educación padre",
  MAX_EDUC_PARENTAL="Máx. educación parental",
  MUJER="Mujer", TRABAJA="Trabaja",
  JEFE_OCUPADO="Jefe ocupado", JEFE_FEMENINO="Jefatura femenina",
  NUM_HERMANOS="N° de hermanos", ESTRUCTURA_HOGAR="Hogar biparental",
  CONC_ING_JEFE="Conc. ingreso del jefe"
)

# 2) Helper: media y desvío estándar
stats_block <- function(dat, vars) {
  dat %>%
    summarise(across(all_of(vars),
                     list(mean = ~mean(., na.rm = TRUE),
                          sd   = ~sd(.,   na.rm = TRUE)),
                     .names = "{.col}.{.fn}")) %>%
    pivot_longer(everything(),
                 names_to = c("variable","stat"),
                 names_sep = "\\.",
                 values_to = "value") %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    arrange(factor(variable, levels = vars))
}

# 3) Calcular Total / Asiste / No asiste
tot <- stats_block(df, vars) %>% rename(TOT_mean = mean, TOT_sd = sd)
asi <- stats_block(filter(df, grupo == "Asiste"), vars) %>%
  rename(ASIS_mean = mean, ASIS_sd = sd)
noa <- stats_block(filter(df, grupo == "No asiste"), vars) %>%
  rename(NOAS_mean = mean, NOAS_sd = sd)

# 4) Unir y rotular
tab <- tot %>%
  left_join(asi, by = "variable") %>%
  left_join(noa, by = "variable") %>%
  mutate(Variables = dplyr::coalesce(var_labels[variable], variable)) %>%
  select(Variables, TOT_mean, TOT_sd, ASIS_mean, ASIS_sd, NOAS_mean, NOAS_sd)

# 5) Fila de observaciones
n_total  <- nrow(df)
n_asiste <- sum(df$grupo == "Asiste")
n_no     <- sum(df$grupo == "No asiste")

obs_row <- data.frame(
  Variables = "Observaciones",
  TOT_mean = n_total, TOT_sd = NA,
  ASIS_mean = n_asiste, ASIS_sd = NA,
  NOAS_mean = n_no, NOAS_sd = NA
)

tab_final <- bind_rows(tab, obs_row)

# 6) Redondeo y limpieza de NA en fila de observaciones
# (deja esta parte como la tenías)
tab_final[, -1] <- lapply(tab_final[, -1], function(x) if (is.numeric(x)) round(x, 2) else x)

kbl(
  tab_final,
  format = "latex",               
  booktabs = TRUE,
  align = c("l", rep(c("r","r"), 3)),
  col.names = c("Variables",
                "Media","Desv. est.", "Media","Desv. est.", "Media","Desv. est."),
  na = ""                           # ⬅️ esto oculta los NA en la impresión
) |>
  add_header_above(c(" " = 1, "TOTAL" = 2, "ASISTE" = 2, "NO ASISTE" = 2)) |>
  kable_styling(full_width = FALSE, latex_options = c("hold_position")) |>
  footnote(general = "Fuente: elaboración propia en base a EPH-INDEC.",
           threeparttable = TRUE) |>
  save_kable("tabla_descriptiva_variables.tex") # Esto despues lo edito en LaTex 
