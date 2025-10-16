# === 04_descriptive_data.R ===
library(dplyr)
library(scales)

# Helpers
es_superior      <- function(CH10, CH12) CH10 == 1 & CH12 %in% c(6, 7)   # terciario o universitario
es_joven_17_23  <- function(CH06) CH06 >= 17 & CH06 <= 23

# ---------- (A) Sin ponderar ----------
asisten_uni <- base_raw %>% filter(es_superior(CH10, CH12))
asisten_uni_jovenes <- asisten_uni %>% filter(es_joven_17_23(CH06))

total_asisten_uni  <- nrow(asisten_uni)
jovenes_17_23      <- nrow(asisten_uni_jovenes)
porcentaje_jovenes <- 100 * jovenes_17_23 / total_asisten_uni

cat("\n[Sin ponderar]\n")
cat("🎓 Total que asisten a superior:", total_asisten_uni, "\n")
cat("🧒 17 a 23 años:", jovenes_17_23, "\n")
cat("📊 Porcentaje:", round(porcentaje_jovenes, 2), "%\n")

# ---------- (B) Ponderado por EPH ----------
peso <- "PONDERA_ind"

desc_pond_trimestre <- base_raw %>%
  mutate(
    asiste_sup  = es_superior(CH10, CH12),
    joven_17_23 = es_joven_17_23(CH06),
    w = .data[[peso]]
  ) %>%
  group_by(trimestre) %>%
  summarise(
    w_asiste_sup  = sum(w[asiste_sup], na.rm = TRUE),
    w_joven_sup   = sum(w[asiste_sup & joven_17_23], na.rm = TRUE),
    pct_joven_sup = 100 * w_joven_sup / w_asiste_sup,
    .groups = "drop"
  )

cat("\n[Ponderado por EPH — por trimestre]\n")
for (i in 1:nrow(desc_pond_trimestre)) {
  cat("📅 Trimestre", desc_pond_trimestre$trimestre[i], "\n")
  cat("🎓 Total ponderado que asisten a superior:", comma(round(desc_pond_trimestre$w_asiste_sup[i])), "\n")
  cat("🧒 17 a 23 años ponderados:", comma(round(desc_pond_trimestre$w_joven_sup[i])), "\n")
  cat("📊 Porcentaje ponderado:", round(desc_pond_trimestre$pct_joven_sup[i], 1), "%\n\n")
}

# ---------- (C) Ponderado total (T3+T4) ----------
desc_pond_total <- base_raw %>%
  mutate(
    asiste_sup  = es_superior(CH10, CH12),
    joven_17_23 = es_joven_17_23(CH06),
    w = .data[[peso]]
  ) %>%
  summarise(
    w_asiste_sup  = sum(w[asiste_sup], na.rm = TRUE),
    w_joven_sup   = sum(w[asiste_sup & joven_17_23], na.rm = TRUE),
    pct_joven_sup = 100 * w_joven_sup / w_asiste_sup
  )

cat("\n[Ponderado total (pool T3+T4)]\n")
cat("🎓 Total ponderado que asisten a superior:", comma(round(desc_pond_total$w_asiste_sup)), "\n")
cat("🧒 17 a 23 años ponderados:", comma(round(desc_pond_total$w_joven_sup)), "\n")
cat("📊 Porcentaje ponderado total:", round(desc_pond_total$pct_joven_sup, 1), "%\n")
