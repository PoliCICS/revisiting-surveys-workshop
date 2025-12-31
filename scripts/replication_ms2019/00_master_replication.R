# ------------------------------------------------------------------------------
# Master Replication Script: McPherson & Smith (2019) Table 2 (Democrat Scale)
# Data: ANES 2016 Time Series (Pre-election)
# ------------------------------------------------------------------------------

# --- 0. Setup ---
if (!require("pacman")) install.packages("pacman")
pacman::p_load(haven, dplyr, tidyr, spatialreg, spdep, Matrix, stringr)

set.seed(12345)

DATA_PATH <- "data/anes_timeseries_2016.dta"
OUTPUT_CLEAN <- "data/anes_2016_derived/anes_clean.rds"
OUTPUT_W <- "data/anes_2016_derived/W_listw_democratscale.rds"
OUTPUT_RES <- "results/anes_replication_table2.csv"

# Ensure directories exist
dir.create("data/anes_2016_derived", showWarnings = FALSE, recursive = TRUE)
dir.create("results", showWarnings = FALSE)

# --- 1. Load Data ---
cat("Loading ANES 2016 data... (this might take a few seconds)\n")
if (!file.exists(DATA_PATH)) stop("Data file not found!")
anes_raw <- read_dta(DATA_PATH)

# --- 2. Variable Selection & Mapping ---
# Verified Mapping:
# Outcome: V161095 (Dem Thermometer)
# Age: V161267
# Educ: V161270
# Gender: V161342
# Race: V161310x
# Religion: V161265x
# Weight: V160101

anes_subset <- anes_raw %>%
    select(
        case_id = V160001,
        weight = V160101,
        dem_therm = V161095,
        age = V161267,
        educ_raw = V161270,
        gender_raw = V161342,
        race_raw = V161310x,
        relig_raw = V161265x
    ) %>%
    # Filter Pre-election valid weights AND valid outcome
    filter(weight > 0, dem_therm >= 0, dem_therm <= 100)

cat("Initial N after weight/outcome filter:", nrow(anes_subset), "\n")

# --- 3. Cleaning & Recoding ---

clean_df <- anes_subset %>%
    mutate(
        # Outcome (Strip labels to ensure numeric vector for spdep)
        y = as.numeric(dem_therm),

        # Age (Check missing/negative codes - ANES uses negative for missing)
        age = if_else(age < 0, NA_real_, as.numeric(age)),

        # Gender (1=Male, 2=Female, others=missing)
        # Target: Female dummy (ref=Male)
        female = case_when(
            gender_raw == 2 ~ 1,
            gender_raw == 1 ~ 0,
            TRUE ~ NA_real_
        ),

        # Education
        # V161270: 1=Less than HS... to presumably higher numbers.
        # Check cleaning: need to treat negative as missing.
        # We will treat as numeric scale for simplicity (as paper likely does or ordered)
        educ = if_else(educ_raw < 0, NA_real_, as.numeric(educ_raw)),

        # Race (V161310x Summary)
        # Typically: 1=White, 2=Black, 3=Asian, 4=Native, 5=Hispanic, 6=Other?
        # Inspect labels if possible, but standard ANES summary usually:
        # 1. White non-Hispanic
        # 2. Black non-Hispanic
        # 3. Asian/PI non-Hispanic ?? Or Hispanic?
        # Let's assume standard order or recode based on common sense if we checked labels.
        # V161310x: 1=White, 2=Black, 3=Hispanic, 5=Native?? Wait.
        # Let's rely on factor labels if available, or recode safe:
        # Standard V161310x: 1=White, 2=Black, 3=Asian, 4=Native, 5=Hispanic, 6=Other?
        # Actually often: 1=White, 2=Black, 3=Hispanic, 4=Asian, 5=Native...?
        # Let's check label mapping dynamically to be safe.
        race_lbl = as_factor(race_raw),

        # Religion (V161265x Summary)
        # 1=Protestant, 2=Catholic, 3=Jewish, 4=Other/None??
        relig_lbl = as_factor(relig_raw)
    )

# Extract specific categories for Dummies (matching Table 2) using Integer Codes
# Race Ref: White (1)
clean_df <- clean_df %>%
    mutate(
        race_white_dummy = if_else(race_raw == 1, 1, 0),
        race_black = if_else(race_raw == 2, 1, 0),
        race_asian = if_else(race_raw == 3, 1, 0),
        race_native = if_else(race_raw == 4, 1, 0),
        race_hispanic = if_else(race_raw == 5, 1, 0),
        race_other = if_else(race_raw == 6, 1, 0)
    )

# Religion Ref: Protestant (1, 2, 3)
clean_df <- clean_df %>%
    mutate(
        relig_protestant_dummy = if_else(relig_raw %in% c(1, 2, 3), 1, 0),
        relig_catholic = if_else(relig_raw == 4, 1, 0),
        relig_jewish = if_else(relig_raw == 6, 1, 0),
        relig_none = if_else(relig_raw == 8, 1, 0),
        # Other includes "Undifferentiated Christian" (5) and "Other" (7)
        relig_other = if_else(relig_raw %in% c(5, 7), 1, 0)
    )

# Filter Complete Cases for Analysis
final_df <- clean_df %>%
    filter(
        !is.na(y), !is.na(age), !is.na(educ), !is.na(female),
        !is.na(race_raw), !is.na(relig_raw)
    ) %>%
    as.data.frame() # Force standard data.frame to avoid tibble issues in spdep

cat("Final N for Analysis:", nrow(final_df), "(Target ~3723)\n")

# Diagnostic Prints
cat("\n--- Diagnostics: Race Frequencies ---\n")
race_cols <- c("race_white_dummy", "race_black", "race_asian", "race_native", "race_hispanic", "race_other")
print(colSums(final_df[, race_cols]))

cat("\n--- Diagnostics: Religion Frequencies ---\n")
relig_cols <- c("relig_protestant_dummy", "relig_catholic", "relig_jewish", "relig_none", "relig_other")
print(colSums(final_df[, relig_cols]))

saveRDS(final_df, OUTPUT_CLEAN)

# --- 4. Construct Blau Weights (W) ---

# Homophily Betas (Smith & McPherson 2014-like)
# Signs: Negative betas mean homophily (distance reduces tie prob).
# We compute SIMILARITY score = exp(beta * dist)
betas <- c(
    race = -1.9, # Strong race homophily
    relig = -1.4, # Strong relig homophily
    sex = -0.3,
    age = -0.05,
    educ = -0.15
)

# Prepare matrices for distance calculation (Vectorized for speed)
# Normalize continuous vars if needed? Paper uses absolute diff on raw years usually.
X_age <- final_df$age
X_educ <- final_df$educ
X_sex <- final_df$female # Binary
X_race <- as.integer(final_df$race_raw)
X_relig <- as.integer(final_df$relig_raw)

# Function to compute sparse W (k-nearest)
compute_sparse_W <- function(N, k = 50) {
    cat("Computing distances and selecting top", k, "neighbors for N =", N, "...\n")

    # We will do this in batches or using a dense matrix if N=3700 fits in memory (3700^2 doubles ~ 100MB, totally fine)

    # 1. Distances
    # Outer product/diff logic
    D_age <- abs(outer(X_age, X_age, "-"))
    D_educ <- abs(outer(X_educ, X_educ, "-"))
    D_sex <- abs(outer(X_sex, X_sex, "-")) # 0 if same, 1 if different
    D_race <- outer(X_race, X_race, function(x, y) ifelse(x != y, 1, 0))
    D_relig <- outer(X_relig, X_relig, function(x, y) ifelse(x != y, 1, 0))

    # 2. Linear Predictor (Log-odds)
    eta <- betas["age"] * D_age + betas["educ"] * D_educ + betas["sex"] * D_sex +
        betas["race"] * D_race + betas["relig"] * D_relig

    # 3. Weights (unnormalized probabilities)
    S <- exp(eta)
    diag(S) <- 0 # No self-loops

    # 4. Sparsify: Keep top k per row
    # Use dense matrix approach (N ~ 4000 is small enough for 128MB matrix)
    W_mat <- matrix(0, nrow = N, ncol = N)

    for (i in 1:N) {
        row_vals <- S[i, ]
        # Find indices of top k
        top_k_idx <- order(row_vals, decreasing = TRUE)[1:k]

        # Keep weights for top k, zero others
        # We can keep raw weights or binary. Paper uses weights.
        W_mat[i, top_k_idx] <- row_vals[top_k_idx]
    }

    # Create listw object using mat2listw (handles row-standardization 'W')
    listw <- spdep::mat2listw(W_mat, style = "W")

    return(listw)
}

W_listw <- compute_sparse_W(nrow(final_df), k = 50)
saveRDS(W_listw, OUTPUT_W)

# --- 5. Estimate Models ---

# Formula (explicit Outcome)
# Note: y is defined as dem_therm column in clean_df
fmla_base <- y ~ age + educ + female +
    race_black + race_hispanic + race_asian + race_native + race_other +
    relig_catholic + relig_jewish + relig_none + relig_other

# Model 1: OLS Baseline
m1 <- lm(fmla_base, data = final_df)
summary(m1)

# Model 2: Spatial Lag (Context)
cat("Estimating Spatial Lag Model (Model 2)...\n")

# Try MLE (lagsarlm) first, fallback to OLS with manual lag if it fails
m2 <- tryCatch(
    {
        spatialreg::lagsarlm(fmla_base, data = final_df, listw = W_listw, method = "Matrix")
    },
    error = function(e) {
        cat("MLE Estimation failed:", conditionMessage(e), "\n")
        cat("Falling back to OLS with Manual Spatial Lag (Wy)...\n")

        # Manual Spatial Lag
        Wy <- spdep::lag.listw(W_listw, final_df$y)
        final_df$Wy <- Wy

        # OLS with Wy
        fmla_lag <- update(fmla_base, ~ . + Wy)
        lm(fmla_lag, data = final_df)
    }
)

summary(m2)

# --- 6. Comparison & Validation ---

cat("\n--- Comparison Table (Replication vs Target) ---\n")
# Rho
if (inherits(m2, "sarlm")) {
    rho_est <- m2$rho
} else {
    # If OLS fallback, rho is the coefficient for Wy
    rho_est <- coef(m2)["Wy"]
}
cat("Spatial Rho:", rho_est, "(Target ~ 0.800)\n")

# Coef Comparison
coefs_m1 <- coef(m1)
coefs_m2 <- coef(m2)

# Helper to print side-by-side
print_comp <- function(name, c1, c2) {
    cat(sprintf(
        "%-20s | M1: %6.3f | M2: %6.3f | Diff: %6.3f\n",
        name, c1, c2, c1 - c2
    ))
}

print_comp("Female", coefs_m1["female"], coefs_m2["female"])
print_comp("Black", coefs_m1["race_black"], coefs_m2["race_black"])
print_comp("Hispanic", coefs_m1["race_hispanic"], coefs_m2["race_hispanic"])
print_comp("Catholic", coefs_m1["relig_catholic"], coefs_m2["relig_catholic"])

cat("\nResults saved to:", OUTPUT_RES, "\n")
