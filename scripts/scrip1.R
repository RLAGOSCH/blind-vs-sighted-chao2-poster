

rm(list = ls())

# cargar librerias
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(forcats)


# funciones in house para el calculo de los estimadores
source("funciones/fGetQ_V2.R")
source("funciones/fShat_bc.R")
source("funciones/fCoverage_V2.R")

# Parámetros
alpha <- 0.05
z <- qnorm(1 - alpha/2)

# cargar normas Blind
dfData <- read_xlsx(path = "data/Datos Italianos_sinRep.xlsx",sheet = "production-data", range = "A1:D36150")

# separar por grupo
dfS <- dfData[dfData$Group == 's',]
dfB <- dfData[dfData$Group == "b",]

# calcular número de sujetos
nT_S <- length(unique(dfS$Subject))
nT_B <- length(unique(dfB$Subject))               

# calcular probabilidades de dectabilidad por concepto en cada grupo
lstVf_S <- tapply(dfS$FeatureEn, dfS$ConceptEn, FUN = table)
lstVf_B <- tapply(dfB$FeatureEn, dfB$ConceptEn, FUN = table)

# calcular el vector de Qk por concpeto para cada grupo
lstQk_S <- lapply(lstVf_S, fGetQ_V2, nT = nT_S)
lstQk_B <- lapply(lstVf_B, fGetQ_V2, nT = nT_B)

# optener el estiamodr de Schao2BC por grupo
lstSchao2BC_S <- lapply(lstQk_S, fShat_bc)
lstSchao2BC_B <- lapply(lstQk_B, fShat_bc)

# optener el coverage por grupo y concpetos

lstCV_S <- lapply(lstQk_S, fCoverage_V2, nT = nT_S)
lstCV_B <- lapply(lstQk_B, fCoverage_V2, nT = nT_B)

# orenar dataframe de coverage (de chao2)

dfCV_S <-  do.call(rbind, lstCV_S)
dfCV_B <-  do.call(rbind, lstCV_B)

dfCV_S <- as.data.frame(dfCV_S)
dfCV_B <- as.data.frame(dfCV_B)

dfCV_S[] <- lapply(dfCV_S, unlist)
dfCV_B[] <- lapply(dfCV_B, unlist)

dfCV_S$Concepto <- row.names(dfCV_S)
dfCV_B$Concepto <- row.names(dfCV_B)

rownames(dfCV_S) <- NULL
rownames(dfCV_B) <- NULL

names(dfCV_S)[1] <- "CV_videntes"
names(dfCV_B)[1] <- "CV_Ciegos"

dfCV <- merge(x = dfCV_B, dfCV_S, by = "Concepto")

dfCV$Diff_CV <-abs( dfCV$CV_videntes - dfCV$CV_Ciegos)

# order data frame de estiamdores
dfSchao2BC_S <- do.call(rbind, lstSchao2BC_S)
dfSchao2BC_B <- do.call(rbind, lstSchao2BC_B)

dfSchao2BC_S <- as.data.frame(dfSchao2BC_S)
dfSchao2BC_B <- as.data.frame(dfSchao2BC_B)

dfSchao2BC_S[] <- lapply(dfSchao2BC_S, unlist)
dfSchao2BC_B[] <- lapply(dfSchao2BC_B, unlist)

dfSchao2BC_S$Concepto <- rownames(dfSchao2BC_S)
dfSchao2BC_B$Concepto <- rownames(dfSchao2BC_B)

rownames(dfSchao2BC_S) <- NULL
rownames(dfSchao2BC_B) <- NULL

# juntar en solo data frame

dfSchao2BC_S$Grupo <- "videntes"
dfSchao2BC_B$Grupo <- "ciegos"

dfDataIC <- rbind(dfSchao2BC_S, dfSchao2BC_B)

# homogenizar nomobre

names(dfDataIC)[1] <- "Schao2BC"

# caclular sd

dfDataIC$SE_Schao2BC <- sqrt(dfDataIC$varShat_BC)

# trafromar data en formato wide

dfwideIC <- dfDataIC %>%
  mutate(Grupo = tolower(Grupo)) %>%
  filter(Grupo %in% c("ciegos", "videntes")) %>%
  select(Concepto, Grupo, Schao2BC, SE_Schao2BC) %>%
  pivot_wider(
    names_from = Grupo,
    values_from = c(Schao2BC, SE_Schao2BC),
    names_sep = "_"
  )

# 2) Diferencia (videntes - ciegos) + IC asintótico y p-value (aprox normal)
dfDiff <- dfwideIC %>%
  transmute(
    Concepto,
    Schao2BC_videntes, SE_videntes = SE_Schao2BC_videntes,
    Schao2BC_ciegos,   SE_ciegos   = SE_Schao2BC_ciegos,
    diff = Schao2BC_videntes - Schao2BC_ciegos,
    SE_diff = sqrt(SE_videntes^2 + SE_ciegos^2),   # independencia entre grupos
    z_stat = diff / SE_diff,
    p_value = 2 * pnorm(-abs(z_stat)),
    CI_low  = diff - z * SE_diff,
    CI_high = diff + z * SE_diff
  ) %>%
  arrange(p_value)


# 3) Ajuste por comparaciones múltiples (elige uno)
#    - Holm (conservador sin suponer independencia; tu favorito cuando hay pocas comparaciones clave)
#    - BH (FDR) si te interesa controlar tasa de falsos descubrimientos
dfDiff <- dfDiff %>%
  mutate(
    p_holm = p.adjust(p_value, method = "holm"),
    p_bh   = p.adjust(p_value, method = "BH"),
    p_FDR   = p.adjust(p_value, method = "fdr"),
    sig_95 = ifelse(CI_low > 0 | CI_high < 0, "No traslape con 0", "Incluye 0")
  )



# fores plot 

# 4) Forest plot (diferencia vid - cie): a la derecha de 0 favorece "videntes"
dfPlotIC <- dfDiff %>%
  mutate(Concepto = fct_reorder(Concepto, diff))


# agregarle al data frame plot los CV para inclirlo en el grafico
dfPlotIC <- merge(x = dfPlotIC, dfCV_S, by = "Concepto")
dfPlotIC <- merge(x = dfPlotIC, dfCV_B, by = "Concepto")

# agregar un boleano que muestre que ambos coveragres son mayores a 0.7
dfPlotIC$CV_07 <- dfPlotIC$CV_Ciegos >=  0.7 & dfPlotIC$CV_videntes >= 0.7


# fores plot 
ggplot(dfPlotIC, aes(x = diff, y = Concepto, color = CV_07)) +
  geom_point(size = 2) +
  geom_errorbarh(
    aes(xmin = CI_low, xmax = CI_high),
    height = 0, size = 0.7
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(
    values = c(`FALSE` = "#000000",  # black
               `TRUE`  = "#E69F00"), # naranja
    name   = "Sample coverage ≥ 0.7",
    labels = c("No", "Yes")
  ) +
  labs(
    x = "Difference in richness Chao2BC (sighted – blind)",
    y = "Concept",
    title = "Comparison by concept with asymptotic CI (Chao2BC)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


# graficar las probabilidades de dectabilidad para un concpeto de ejemplo

# escojamos al concetpo mas bkn de todo DOG


dfDog <- dfData[dfData$ConceptEn == "dog",]

dfDog <- dfDog[,c("Subject",  "ConceptEn", "FeatureEn")]


# frecuencia


dfFeatures_Dog <- as.data.frame(table(dfDog$FeatureEn))


dfFeatures_Dog <- dfFeatures_Dog[order(-dfFeatures_Dog$Freq), ]

row.names(dfFeatures_Dog) <- NULL

dfFeatures_Dog$Rank <- 1:dim(dfFeatures_Dog)[1]


ggplot(dfFeatures_Dog, aes(x = Rank, y = Freq)) +
  geom_line(color = "#E69F00", size = 1) +
  geom_area(fill = "#E69F00", alpha = 0.3) +
  labs(
    title = "Rank–frequency distribution (concept: dog)",
    x = "Rank",
    y = "Frequency"
  ) +
  theme_minimal()+
  theme(
    panel.grid = element_blank(),            # sin grilla
    axis.line = element_line(color = "black"), # ejes visibles
    axis.ticks = element_line(color = "black")
  ) +
  scale_y_continuous(expand = c(0, 0)) +      # curva toca eje X
  scale_x_continuous(expand = c(0, 0))        # curva toca eje Y

