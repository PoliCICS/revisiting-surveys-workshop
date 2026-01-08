# ------------------------------------------------------------------------------
# Script: 00_master_replication_v4.R
# Purpose: Validating the switch from OLS-spatial-lag to SAR (spatialreg::lagsarlm)
#          to replicate McPherson & Smith (2019).
#          ROBUST VERSION: Explicit analytic sample, safe listw, impacts interpretation.
# ------------------------------------------------------------------------------

# 1. Setup & Libraries =========================================================
suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(spdep)
    library(spatialreg)
})

cat(">>> Loaded libraries.\n")

data_path <- "data/anes_2016_derived/anes_clean.rds"
if (!file.exists(data_path)) {
    stop("Data file not found at: ", data_path)
}

final_df <- readRDS(data_path)
cat(">>> Loaded ANES data. Initial N =", nrow(final_df), "\n")

# 2. Define Analytic Sample EXPLICITLY =========================================
# CRITICAL: We must drop NAs BEFORE building W so that listw matches the regression data exactly.
# spatialreg manual warns against subsetting listw built with glist if NAs are removed by the model.

vars_needed <- c(
    "y", "age", "educ", "female",
    "race_black", "race_hispanic", "race_asian", "race_native", "race_other",
    "relig_catholic", "relig_jewish", "relig_none", "relig_other",
    "race_raw", "relig_raw"
) # Raw vars used for W construction

cat(">>> Creating analytic sample (dropping NAs)...\n")
df_model <- final_df %>%
    select(all_of(vars_needed)) %>%
    drop_na()

cat(">>> Analytic N =", nrow(df_model), "( Dropped", nrow(final_df) - nrow(df_model), "rows )\n\n")


# 3. Re-construct W Matrix on Analytic Sample ==================================
# Using df_model variables to ensure alignment.

X_age <- df_model$age
X_educ <- df_model$educ
X_sex <- df_model$female
X_race <- as.integer(df_model$race_raw)
X_relig <- as.integer(df_model$relig_raw)

# Betas (McPherson & Smith 2019 / GSS 2004 replication values)
betas <- c(race = -1.352, relig = -1.354, sex = -0.256, age = -0.047, educ = -0.189)

cat(">>> Calculating distances in Blau Space (Analytic Sample)...\n")
D_age <- abs(outer(X_age, X_age, "-"))
D_educ <- abs(outer(X_educ, X_educ, "-"))
D_sex <- abs(outer(X_sex, X_sex, "-"))
D_race <- outer(X_race, X_race, function(x, y) ifelse(x != y, 1, 0))
D_relig <- outer(X_relig, X_relig, function(x, y) ifelse(x != y, 1, 0))

eta <- betas["age"] * D_age + betas["educ"] * D_educ + betas["sex"] * D_sex +
    betas["race"] * D_race + betas["relig"] * D_relig

w_raw <- exp(eta)
diag(w_raw) <- 0

# Selection Top-K
N <- nrow(df_model)
K <- min(100, N - 1) # Safety check
neighbors <- vector("list", N)
weights_list <- vector("list", N)

cat(sprintf(">>> Selecting Top-K neighbors (K=%d)...\n", K))
for (i in 1:N) {
    row_w <- w_raw[i, ]
    ord <- order(row_w, decreasing = TRUE)
    candidates <- setdiff(ord, i)

    top_k_indices <- candidates[1:K]
    top_k_weights <- row_w[top_k_indices]

    # CRITICAL: Sort indices for valid nb structure.
    # unsorted 'nb' lists often cause errors in sparse matrix conversion (impacts, etc.)
    sort_idx <- order(top_k_indices)

    neighbors[[i]] <- top_k_indices[sort_idx]
    weights_list[[i]] <- top_k_weights[sort_idx]
}

class(neighbors) <- "nb"
attr(neighbors, "region.id") <- as.character(1:N)
attr(neighbors, "call") <- match.call()

cat(">>> Checking W symmetry...\n")
sym_nb <- spdep::is.symmetric.nb(neighbors)
cat("    Neighbor graph symmetric:", sym_nb, "\n")
if (!sym_nb) {
    warning("Top-K neighbor graph is not symmetric (directed). This implies W is not symmetric.")
}

W_listw <- nb2listw(neighbors, glist = weights_list, style = "W", zero.policy = TRUE)
cat(">>> W matrix constructed on analytic sample.\n\n")


# 4. Fit Models (Consistent Sample) ============================================

fmla_base <- y ~ age + educ + female +
    race_black + race_hispanic + race_asian + race_native + race_other +
    relig_catholic + relig_jewish + relig_none + relig_other

# --- Model 1: OLS ---
cat(">>> Estimating Model 1 (OLS)...\n")
m1 <- lm(fmla_base, data = df_model)

# --- Model 2: SAR (Spatial Autoregressive) ---
cat(">>> Estimating Model 2 (SAR)...\n")
start_time <- Sys.time()

# Robust fit using tryCatch for method fallback
m2_sar <- tryCatch(
    {
        spatialreg::lagsarlm(
            formula     = fmla_base,
            data        = df_model,
            listw       = W_listw,
            zero.policy = TRUE,
            method      = "eigen",
            na.action   = na.fail # SAFETY: Prevent internal subsetting
        )
    },
    error = function(e) {
        message("    Method 'eigen' failed: ", e$message)
        message("    Switching to method 'Matrix' (sparse matrix decomposition)...")
        spatialreg::lagsarlm(
            formula     = fmla_base,
            data        = df_model,
            listw       = W_listw,
            zero.policy = TRUE,
            method      = "Matrix",
            na.action   = na.fail
        )
    }
)
end_time <- Sys.time()
sar_duration <- end_time - start_time

# Summary Extraction (Robust)
summary_sar <- summary(m2_sar)

cat("\n--- SAR Model Diagnostics ---\n")
if (!is.null(summary_sar$LR)) {
    # Robust printing for LR test object
    if (is.list(summary_sar$LR)) {
        cat("LR Test Statistic:", summary_sar$LR$statistic, "\n")
        cat("LR Test p-value:  ", summary_sar$LR$p.value, "\n")
    } else {
        print(summary_sar$LR)
    }
}
cat("Rho (Spatial Autoregressive):", m2_sar$rho, "\n")
cat(sprintf("SAR Model Execution Time: %.2f %s\n", sar_duration, units(sar_duration)))

# 4b. Coefficient Comparison (Printed) =========================================
cat("\n====================================================================\n")
cat("COEFFICIENT COMPARISON (OLS vs SAR)\n")
cat("====================================================================\n")

# Extract coefficients
coef_m1 <- coef(m1)
coef_m2 <- coef(m2_sar)

# Align names (SAR has rho, but coef() typically returns betas sometimes with rho depending on package version,
# usually coef(m2) is just betas, rho is separate or at end. standard lagsarlm coef includes betas.)
# Let's align by common names.
common_names <- intersect(names(coef_m1), names(coef_m2))

df_coef <- data.frame(
    Variable = common_names,
    OLS = coef_m1[common_names],
    SAR = coef_m2[common_names]
)
df_coef$Diff <- df_coef$OLS - df_coef$SAR
df_coef$Pct_Change <- (df_coef$Diff / abs(df_coef$OLS)) * 100

print(df_coef, digits = 3, row.names = FALSE)


# 5. Validation Check ==========================================================
cat("\n====================================================================\n")
cat("VALIDATION CHECK: Comparing with McPherson & Smith (2019) Table 2\n")
cat("====================================================================\n")

target_rho <- 0.800
target_m2_intercept <- 1.121

my_intercept <- coef(m2_sar)["(Intercept)"]
my_rho <- m2_sar$rho

cat(sprintf("Analytic N    | %d\n", nrow(df_model)))
if (nrow(df_model) != 3723 && nrow(df_model) != 3163) { # 3723 is paper N, 3163 might be our clean N
    cat("NOTE: Sample size may differ from paper due to cleaning/NA handling differences.\n")
}

cat(sprintf("----------------------------------------------------\n"))
cat(sprintf("Parameter     | Paper Value | My Estimate | Diff\n"))
cat(sprintf("----------------------------------------------------\n"))
cat(sprintf("Rho           | %11.3f | %11.3f | %11.3f\n", target_rho, my_rho, my_rho - target_rho))
cat(sprintf("Intercept     | %11.3f | %11.3f | %11.3f\n", target_m2_intercept, my_intercept, my_intercept - target_m2_intercept))

# 6. Interpretation: Direct/Indirect Impacts ===================================
cat("\n\n>>> Calculating Impacts (Direct vs Indirect)...\n")
cat("    (SAR coefficients != marginal effects due to feedback loops)\n")

set.seed(123) # Reproducibility for Monte Carlo z-stats
imp <- spatialreg::impacts(m2_sar, listw = W_listw, R = 200)
print(summary(imp, zstats = TRUE, short = TRUE))


# 7. Output Table ==============================================================
if (requireNamespace("texreg", quietly = TRUE)) {
    cat("\n>>> Comparison Table (texreg):\n")
    print(texreg::screenreg(list(m1, m2_sar),
        custom.model.names = c("OLS (Base)", "SAR (Context)")
    ))
} else {
    cat("\n(texreg not installed, skipping table)\n")
}

cat("\nDone.\n")
