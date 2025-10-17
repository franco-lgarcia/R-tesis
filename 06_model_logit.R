# === 06_model_logit.R ===
library(dplyr)
library(sandwich)
library(lmtest)
library(modelsummary)

# -------------------------------------------------------------------------
# 0) Preparación
# -------------------------------------------------------------------------
# 0) chequeo de tamaños
stopifnot(nrow(base_modelo) == nrow(base_filtrada))

# 1) armo dfc pegando IDs + ponderador por posición
dfc <- dplyr::bind_cols(
  base_filtrada %>% dplyr::select(CODUSU, NRO_HOGAR, trimestre, PONDERA_ind),
  base_modelo
)

# 2) construir variables centradas y peso normalizado
dfc <- dfc %>%
  dplyr::filter(complete.cases(.)) %>% 
  dplyr::mutate(
    EDADc  = EDAD - mean(EDAD, na.rm = TRUE),
    EDAD2c = EDADc^2,
    w = PONDERA_ind / mean(PONDERA_ind, na.rm = TRUE)
  )

# -------------------------------------------------------------------------
# 1) Definición de fórmulas
# -------------------------------------------------------------------------
# Modelo A: incluye madre y padre, sin max parental
fA <- ASISTE_SUPERIOR ~ EDADc + EDAD2c + MUJER + TRABAJA +
  EDUC_MADRE + EDUC_PADRE +
  JEFE_OCUPADO + JEFE_FEMENINO + NUM_HERMANOS +
  ESTRUCTURA_HOGAR + CONC_ING_JEFE +
  LOG_ING_PC + REGION

# Modelo B: incluye solo max parental
fB <- ASISTE_SUPERIOR ~ EDADc + EDAD2c + MUJER + TRABAJA +
  MAX_EDUC_PARENTAL +
  JEFE_OCUPADO + JEFE_FEMENINO + NUM_HERMANOS +
  ESTRUCTURA_HOGAR + CONC_ING_JEFE +
  LOG_ING_PC + REGION

# -------------------------------------------------------------------------
# 2) Estimación logit con pesos normalizados
# -------------------------------------------------------------------------
mA <- glm(fA, data = dfc, family = binomial("logit"), weights = w)
mB <- glm(fB, data = dfc, family = binomial("logit"), weights = w)

# -------------------------------------------------------------------------
# 3) Errores estándar robustos
# -------------------------------------------------------------------------
vcA <- sandwich::vcovHC(mA, type = "HC1")
vcB <- sandwich::vcovHC(mB, type = "HC1")

# -------------------------------------------------------------------------
# 4) Etiquetas legibles para la salida
# -------------------------------------------------------------------------
coef_map <- c(
  "(Intercept)"        = "Constante",
  "EDADc"              = "Edad (centrada)",
  "EDAD2c"             = "Edad² (centrada)",
  "MUJER"              = "Mujer (=1)",
  "TRABAJA"            = "Trabaja (=1)",
  "EDUC_MADRE"         = "Educación madre (años)",
  "EDUC_PADRE"         = "Educación padre (años)",
  "MAX_EDUC_PARENTAL"  = "Máx. educación parental (años)",
  "JEFE_OCUPADO"       = "Jefe ocupado (=1)",
  "JEFE_FEMENINO"      = "Jefatura femenina (=1)",
  "NUM_HERMANOS"       = "N° hermanos convivientes",
  "ESTRUCTURA_HOGAR"   = "Hogar biparental (=1)",
  "CONC_ING_JEFE"      = "Conc. ingreso del jefe",
  "LOG_ING_PC"         = "Log ingreso per cápita",
  "REGIONPampeana"     = "Región: Pampeana",
  "REGIONNOA"          = "Región: NOA",
  "REGIONNEA"          = "Región: NEA",
  "REGIONCuyo"         = "Región: Cuyo",
  "REGIONPatagonia"    = "Región: Patagonia"
)

# -------------------------------------------------------------------------
# 5) Exportar tabla comparativa en LaTeX
# -------------------------------------------------------------------------
modelsummary(
  list("Modelo A: Madre/Padre" = mA,
       "Modelo B: Máx. Parental" = mB),
  vcov     = list(vcA, vcB),
  coef_map = coef_map,
  title    = "Modelos logit alternativos de asistencia a educación superior",
  stars    = c('*' = .1, '**' = .05, '***' = .01),
  gof_map  = c("nobs", "AIC", "BIC", "pseudo.r.squared"),
  fmt      = 3,
  output   = "latex",
  file     = "Cuadro_Logit_Alternativos.tex"
)

cat("✅ Modelos estimados y tabla LaTeX guardada en: Cuadro_Logit_Alternativos.tex\n")
