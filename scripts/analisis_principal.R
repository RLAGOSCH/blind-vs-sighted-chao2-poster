# Limpiar el entorno
rm(list = ls())

# ─────────────────────────────────────────────────────
# Cargar librerías necesarias
# ─────────────────────────────────────────────────────
library(readxl)      # Leer archivos Excel
library(dplyr)       # Manipulación de datos
library(tidyr)       # Transformación wide <-> long
library(stringr)     # Manipulación de strings
library(ggplot2)     # Gráficos
library(forcats)     # Ordenar factores en ggplot

# ─────────────────────────────────────────────────────
# Cargar funciones personalizadas para el análisis
# ─────────────────────────────────────────────────────
source("funciones/fGetQ_V2.R")      # Calcula frecuencias Qk
source("funciones/fShat_bc.R")      # Estima Schao2 corregido por sesgo
source("funciones/fCoverage_V2.R")  # Estima sample coverage

# ─────────────────────────────────────────────────────
# Parámetros generales para IC
# ─────────────────────────────────────────────────────
alpha <- 0.05
z <- qnorm(1 - alpha / 2)  # Valor crítico z para IC del 95%

# ─────────────────────────────────────────────────────
# Leer datos desde el Excel original
# ─────────────────────────────────────────────────────
df_raw <- read_xlsx("data/Datos Italianos.xlsx",
                    sheet = "production-data",
                    range = "A1:D36150")

# ─────────────────────────────────────────────────────
# Separar los datos en dos grupos: videntes y ciegos
# ─────────────────────────────────────────────────────
df_sighted <- df_raw %>% filter(Group == "s")
df_blind   <- df_raw %>% filter(Group == "b")

# Número total de participantes por grupo
n_sighted <- n_distinct(df_sighted$Subject)
n_blind   <- n_distinct(df_blind$Subject)

# ─────────────────────────────────────────────────────
# Contar cuántas veces aparece cada característica por concepto
# ─────────────────────────────────────────────────────
lst_feat_sighted <- tapply(df_sighted$FeatureEn, df_sighted$ConceptEn, table)
lst_feat_blind   <- tapply(df_blind$FeatureEn, df_blind$ConceptEn, table)

# ─────────────────────────────────────────────────────
# Obtener los vectores Qk (frecuencia de aparición por concepto)
# ─────────────────────────────────────────────────────
lst_qk_sighted <- lapply(lst_feat_sighted, fGetQ_V2, nT = n_sighted)
lst_qk_blind   <- lapply(lst_feat_blind,   fGetQ_V2, nT = n_blind)

# ─────────────────────────────────────────────────────
# Estimar riqueza semántica Schao2BC y coverage
# ─────────────────────────────────────────────────────
lst_schao2_sighted <- lapply(lst_qk_sighted, fShat_bc)
lst_schao2_blind   <- lapply(lst_qk_blind,   fShat_bc)

lst_cov_sighted <- lapply(lst_qk_sighted, fCoverage_V2, nT = n_sighted)
lst_cov_blind   <- lapply(lst_qk_blind,   fCoverage_V2, nT = n_blind)

# ─────────────────────────────────────────────────────
# Convertir listas de coverage a data frames (2 columnas)
# ─────────────────────────────────────────────────────
df_cov_sighted <- data.frame(
  Concepto = names(lst_cov_sighted),
  CV_videntes = unlist(lst_cov_sighted)
)

df_cov_blind <- data.frame(
  Concepto = names(lst_cov_blind),
  CV_ciegos = unlist(lst_cov_blind)
)

df_coverage <- left_join(df_cov_blind, df_cov_sighted, by = "Concepto") %>%
  mutate(Diff_CV = abs(CV_videntes - CV_ciegos))

rm(lst_cov_sighted, lst_cov_blind)  # limpieza

# ─────────────────────────────────────────────────────
# Unir estimadores en un solo data frame largo
# ─────────────────────────────────────────────────────
df_schao2_sighted <- bind_rows(lst_schao2_sighted, .id = "Concepto") %>%
  mutate(Grupo = "videntes")

df_schao2_blind <- bind_rows(lst_schao2_blind, .id = "Concepto") %>%
  mutate(Grupo = "ciegos")

df_estimadores <- bind_rows(df_schao2_sighted, df_schao2_blind) %>%
  rename(Schao2BC = Shat_BC,
         SE_Schao2BC = varShat_BC) %>%
  mutate(SE_Schao2BC = sqrt(SE_Schao2BC))

rm(lst_schao2_sighted, lst_schao2_blind,
   df_schao2_sighted, df_schao2_blind)

# ─────────────────────────────────────────────────────
# Convertir a formato wide para comparar entre grupos
# ─────────────────────────────────────────────────────
df_wide <- df_estimadores %>%
  mutate(Grupo = tolower(Grupo)) %>%
  select(Concepto, Grupo, Schao2BC, SE_Schao2BC) %>%
  pivot_wider(names_from = Grupo, values_from = c(Schao2BC, SE_Schao2BC))

# ─────────────────────────────────────────────────────
# Comparación entre grupos: diferencia, IC y p-valor
# ─────────────────────────────────────────────────────
df_diff <- df_wide %>%
  transmute(
    Concepto,
    Schao2BC_videntes,
    Schao2BC_ciegos,
    SE_videntes = SE_Schao2BC_videntes,
    SE_ciegos   = SE_Schao2BC_ciegos,
    diff        = Schao2BC_videntes - Schao2BC_ciegos,
    SE_diff     = sqrt(SE_videntes^2 + SE_ciegos^2),
    z_stat      = diff / SE_diff,
    p_value     = 2 * pnorm(-abs(z_stat)),
    CI_low      = diff - z * SE_diff,
    CI_high     = diff + z * SE_diff
  ) %>%
  arrange(p_value)

# ─────────────────────────────────────────────────────
# Corrección por múltiples comparaciones
# ─────────────────────────────────────────────────────
df_diff <- df_diff %>%
  mutate(
    p_holm = p.adjust(p_value, method = "holm"),
    p_bh   = p.adjust(p_value, method = "BH"),
    p_FDR  = p.adjust(p_value, method = "fdr"),
    sig_95 = ifelse(CI_low > 0 | CI_high < 0, "No traslape con 0", "Incluye 0")
  )

# ─────────────────────────────────────────────────────
# Gráfico tipo forest plot para diferencias significativas
# ─────────────────────────────────────────────────────
df_plot <- df_diff %>%
  left_join(df_cov_sighted, by = "Concepto") %>%
  left_join(df_cov_blind,   by = "Concepto") %>%
  mutate(CV_07 = CV_videntes >= 0.7 & CV_ciegos >= 0.7) %>%
  mutate(Concepto = fct_reorder(Concepto, diff))

ggplot(df_plot, aes(x = diff, y = Concepto, color = CV_07)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0, linewidth = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(
    values = c(`FALSE` = "#000000", `TRUE` = "#E69F00"),
    name   = "Sample coverage ≥ 0.7",
    labels = c("No", "Yes")
  ) +
  labs(
    x = "Diferencia en Schao2BC (videntes – ciegos)",
    y = "Concepto",
    title = "Comparación entre grupos (IC asintótico)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# ─────────────────────────────────────────────────────
# Gráfico rank–frecuencia para el concepto 'dog'
# ─────────────────────────────────────────────────────
df_dog <- df_raw %>%
  filter(ConceptEn == "dog") %>%
  count(FeatureEn, name = "Freq") %>%
  arrange(desc(Freq)) %>%
  mutate(Rank = row_number())

ggplot(df_dog, aes(x = Rank, y = Freq)) +
  geom_line(color = "#E69F00", size = 1) +
  geom_area(fill = "#E69F00", alpha = 0.3) +
  labs(
    title = "Distribución rank–frecuencia (concepto: dog)",
    x = "Rango",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0))

