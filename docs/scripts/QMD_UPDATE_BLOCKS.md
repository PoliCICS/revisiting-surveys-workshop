# PROPOSED QMD UPDATES for `3-network-from-surveys.qmd` (ROBUST VERSION)

These updates ensure the SAR model is estimated on a strictly consistent analytic sample, avoiding errors with `listw` subsetting.

## 1. Update Library Loading (Chunk: `load-anes`)

Add `texreg` for tables.

```r
#| label: load-anes
#| message: false
#| warning: false

library(dplyr)
library(tidyr)
library(spdep)
library(spatialreg)
library(texreg) # For SAR model output

# Cargar datos limpios
final_df_raw <- readRDS("data/anes_2016_derived/anes_clean.rds")
cat("Datos ANES cargados. N (Raw) =", nrow(final_df_raw), "\n")
```

## 2. Analytic Sample & W Construction (Chunk: `construct-w`)

**Crucial Change**: We must define the analytic sample (dropping NAs) *before* building the W matrix to ensure 1:1 correspondence between rows in the data and the weights.

```r
#| label: construct-w
#| message: false

# 1. Definir Muestra Analítica (Drop NAs explicitly)
vars_model <- c("y", "age", "educ", "female",
                 "race_black", "race_hispanic", "race_asian", "race_native", "race_other",
                 "relig_catholic", "relig_jewish", "relig_none", "relig_other",
                 "race_raw", "relig_raw")

final_df <- final_df_raw %>%
  select(all_of(vars_model)) %>%
  drop_na()

cat("Muestra Analítica definida. N =", nrow(final_df), "(Se eliminaron", nrow(final_df_raw) - nrow(final_df), "casos con NA)\n")

# 2. Preparar Datos para Blau Space
X_age <- final_df$age
X_educ <- final_df$educ
X_sex <- final_df$female
X_race <- as.integer(final_df$race_raw)
X_relig <- as.integer(final_df$relig_raw)

# Betas de Homofilia (Basados en Replicación V3 - GSS 2004)
betas <- c(race = -1.352, relig = -1.354, sex = -0.256, age = -0.047, educ = -0.189)

cat("Calculando distancias en Blau Space...\n")
D_age <- abs(outer(X_age, X_age, "-"))
D_educ <- abs(outer(X_educ, X_educ, "-"))
D_sex <- abs(outer(X_sex, X_sex, "-"))
D_race <- outer(X_race, X_race, function(x, y) ifelse(x != y, 1, 0))
D_relig <- outer(X_relig, X_relig, function(x, y) ifelse(x != y, 1, 0))

# Predictor Lineal y Pesos
eta <- betas["age"] * D_age + betas["educ"] * D_educ + betas["sex"] * D_sex +
    betas["race"] * D_race + betas["relig"] * D_relig

w_raw <- exp(eta)
diag(w_raw) <- 0

# Selección Top-K (K=100)
N <- nrow(final_df)
K <- min(100, N - 1)
neighbors <- vector("list", N)
weights_list <- vector("list", N)

cat("Seleccionando Top-K vecinos...\n")
for (i in 1:N) {
    row_w <- w_raw[i, ]
    ord <- order(row_w, decreasing = TRUE)
    candidates <- setdiff(ord, i)
    top_k_indices <- candidates[1:K]
    top_k_weights <- row_w[top_k_indices]
    
    # CRITICAL: Ordenar índices para evitar error dgRMatrix en impacts()
    sort_idx <- order(top_k_indices)
    neighbors[[i]] <- top_k_indices[sort_idx]
    weights_list[[i]] <- top_k_weights[sort_idx]
}

class(neighbors) <- "nb"
attr(neighbors, "region.id") <- as.character(1:N)
attr(neighbors, "call") <- match.call()

# Validar simetría (Advertencia si no es simétrica)
if(!spdep::is.symmetric.nb(neighbors)) {
  cat("Nota: La red de vecinos Top-K es dirigida (no simétrica).\n")
}

# 3. Crear Objeto W (Row-standardized)
W_listw <- nb2listw(neighbors, glist = weights_list, style = "W", zero.policy = TRUE)
cat("Matriz W construida sobre muestra analítica exacta.\n")
```

## 3. Estimate SAR Model (Chunk: `estimate-models`)

Use `lagsarlm` with `na.fail` to prevent any silent subsetting.

```r
#| label: estimate-models
#| message: false
#| results: asis

# Fórmula Base
fmla_base <- y ~ age + educ + female +
    race_black + race_hispanic + race_asian + race_native + race_other +
    relig_catholic + relig_jewish + relig_none + relig_other

# Modelo 1: OLS
m1 <- lm(fmla_base, data = final_df)

# Modelo 2: SAR (Spatial Autoregressive)
# Importante: na.action = na.fail verifica que no haya NAs residuales
# Method = "eigen" es rápido para N < 5000.
m2_sar <- spatialreg::lagsarlm(
    formula     = fmla_base,
    data        = final_df,
    listw       = W_listw,
    zero.policy = TRUE,
    method      = "eigen", 
    na.action   = na.fail
)

# Resultados con texreg (htmlreg para Quarto HTML)
htmlreg(list(m1, m2_sar),
    custom.model.names = c("Base (OLS)", "Contextual (SAR)"),
    caption = "Democrat Scale (ANES 2016) - OLS vs SAR",
    # Mapeo de nombres para visualización limpia
    custom.coef.map = list(
        "(Intercept)" = "Intercept",
        "age" = "Age",
        "educ" = "Education",
        "female" = "Female",
        "race_black" = "Black",
        "race_hispanic" = "Hispanic",
        "race_asian" = "Asian",
        "race_native" = "Native American",
        "race_other" = "Other race",
        "relig_catholic" = "Catholic",
        "relig_jewish" = "Jewish",
        "relig_none" = "None",
        "relig_other" = "Other religion",
        "rho" = "Spatial parameter (rho)"
    ),
    star.symbol = "*",
    center = TRUE,
    doctype = FALSE
)
```

## 4. Interpretation & Impacts (Chunk: `compare-coefs`)

Report Impacts instead of raw coefficients for SAR.

```r
#| label: compare-coefs
#| echo: false

# Extraer Rho
rho_val <- m2_sar$rho
cat("--- Parámetro de Dependencia Espacial ---\n")
# Acceso seguro a LR test
if (!is.null(summary(m2_sar)$LR)) {
    pval <- summary(m2_sar)$LR$p.value
    cat(sprintf("Rho (Autocorrelación): %.3f (LR Test p-value: %.4f)\n\n", rho_val, pval))
} else {
    cat(sprintf("Rho (Autocorrelación): %.3f\n\n", rho_val))
}

# Cálculo de Impactos (Directos vs Indirectos/Spillover)
cat("--- Impactos (Efectos Totales) ---\n")
set.seed(123) # Reproducibilidad para Monte Carlo
imp <- spatialreg::impacts(m2_sar, listw = W_listw, R = 1000)
print(summary(imp, zstats = TRUE, short = TRUE))

# Tabla comparativa simple (Visualización rápida)
# Nota: Para SAR, el 'impacto directo' es más comparable al Beta OLS que el 'coeficiente SAR'.
# Aquí mostramos coeficientes crudos con la advertencia de interpretación.
c1 <- coef(m1)
c2 <- coef(m2_sar)

df_comp <- data.frame(
    Variable = c("Black", "Hispanic", "Catholic", "Female"),
    Model1 = c(c1["race_black"], c1["race_hispanic"], c1["relig_catholic"], c1["female"]),
    Model2 = c(c2["race_black"], c2["race_hispanic"], c2["relig_catholic"], c2["female"])
)

df_comp$Diff_Abs <- df_comp$Model1 - df_comp$Model2
df_comp$Pct_Reduction <- (df_comp$Diff_Abs / df_comp$Model1) * 100

knitr::kable(df_comp[, c("Variable", "Model1", "Model2", "Pct_Reduction")],
    digits = 2,
    caption = "Comparación de Coeficientes (Nota: En SAR, interpretar con Impactos)",
    col.names = c("Variable", "Coef OLS", "Coef SAR", "% Reducción")
)
```
