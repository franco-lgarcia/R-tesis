library(dplyr)
library(tidyr)
library(purrr)
library(modelsummary)

df2 <- base_modelo %>%
  filter(!is.na(ASISTE_SUPERIOR)) %>%
  mutate(ASISTE_SUPERIOR = factor(ASISTE_SUPERIOR, levels = c(1,0),
                                  labels = c("Asiste","No asiste")))

vars <- c("EDAD","EDAD2","MUJER","TRABAJA","EDUC_MADRE","EDUC_PADRE",
          "MAX_EDUC_PARENTAL","JEFE_OCUPADO","JEFE_FEMENINO","NUM_HERMANOS",
          "ESTRUCTURA_HOGAR","CONC_ING_JEFE","LOG_ING_PC")

# helper: calcula mean/sd para un subconjunto y etiqueta columnas
stats_cols <- function(data, prefix){
  data %>%
    summarise(across(all_of(vars),
                     list(Mean = ~mean(., na.rm = TRUE),
                          `Std. Dev.` = ~sd(., na.rm = TRUE)),
                     .names = "{.col}.{.fn}")) %>%
    pivot_longer(everything(),
                 names_to = c("Variable","Stat"), names_sep = "\\.",
                 values_to = "val") %>%
    mutate(Col = paste(prefix, Stat, sep = " ")) %>%
    select(Variable, Col, val) %>%
    pivot_wider(names_from = Col, values_from = val)
}

tot <- stats_cols(df2, "Total")
asi <- stats_cols(filter(df2, ASISTE_SUPERIOR == "Asiste"), "Asiste")
noa <- stats_cols(filter(df2, ASISTE_SUPERIOR == "No asiste"), "No asiste")

tabla_df <- reduce(list(tot, asi, noa), full_join, by = "Variable") %>%
  mutate(Variable = factor(Variable, levels = vars)) %>%
  arrange(Variable)

# render “paper style”
datasummary_df(
  tabla_df,
  title = "Cuadro X. Estadísticas descriptivas por asistencia",
  fmt = 2, output = "gt"
)
