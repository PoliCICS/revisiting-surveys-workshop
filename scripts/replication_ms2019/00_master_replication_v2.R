# -------------------------------------------------------------------------
# Master Replication Script V2: McPherson & Smith (2019) using ANES 2016
# -------------------------------------------------------------------------
# Goal: Replicate Table 2 (Democrat Scale) with "Paper-Like" methodology.
# Changes from V1:
#   1. Weights: Use actual probabilities w_ij \propto exp(beta * d_ij) instead of uniform kNN.
#   2. Neighborhood: K = 100 (Top-K truncation) instead of K = 50.
#   3. Normalization: Proper row-standardization of weighted ties.
#   4. Output: Compact regression table (No Standard Errors).

# -------------------------------------------------------------------------
# 1. Setup & Data Loading
# -------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, spdep, spatialreg, Matrix, stargazer, haven)

# Load clean ANES data (ensure this exists)
DATA_PATH <- "data/anes_2016_derived/anes_clean.rds"
if (!file.exists(DATA_PATH)) stop("Run 'anes-cleaning.qmd' first to generate anes_clean.rds")

final_df <- readRDS(DATA_PATH)
cat("Data Loaded. N =", nrow(final_df), "\n")

# -------------------------------------------------------------------------
# 2. Blau Space Construction (Paper-Like Weights)
# -------------------------------------------------------------------------
# Betas from Smith & McPherson (2014) logic (approximate for 2016 context)
betas <- c(race = -1.9, relig = -1.4, sex = -0.3, age = -0.05, educ = -0.15)

cat("Constructing Blau Weights (K=100, Non-Uniform)...\n")

# Prepare Matrices (N x N dense calculation - OK for N~4000)
X_age <- final_df$age
X_educ <- final_df$educ
X_sex <- final_df$female
X_race <- as.integer(final_df$race_raw)
X_relig <- as.integer(final_df$relig_raw)

# 2.1 Calculate Distances
t0 <- Sys.time()
D_age <- abs(outer(X_age, X_age, "-"))
D_educ <- abs(outer(X_educ, X_educ, "-"))
D_sex <- abs(outer(X_sex, X_sex, "-"))
D_race <- outer(X_race, X_race, function(x, y) ifelse(x != y, 1, 0))
D_relig <- outer(X_relig, X_relig, function(x, y) ifelse(x != y, 1, 0))
cat("Distances computed in:", round(difftime(Sys.time(), t0, units = "secs"), 2), "s\n")

# 2.2 Linear Predictor (Eta) & Raw Weights (Unnormalized Probabilities)
eta <- betas["age"] * D_age +
    betas["educ"] * D_educ +
    betas["sex"] * D_sex +
    betas["race"] * D_race +
    betas["relig"] * D_relig

w_raw <- exp(eta)
diag(w_raw) <- 0 # No self-loops

# 2.3 Top-K Selection (K=100) with WEIGHTED values
K <- 100
N <- nrow(final_df)
neighbors <- vector("list", N)
weights_list <- vector("list", N)

cat("Selecting Top-K neighbors and extracting weights...\n")
# Loop to select top K and strictly preserve their w_raw values
for (i in 1:N) {
    # Sort all j by weight (descending)
    # IMPORTANT: We want the actual INDICES of top K
    row_w <- w_raw[i, ]
    ord <- order(row_w, decreasing = TRUE)

    # Ensure we don't pick self (diag is 0, so unlikely to be top unless sparse, but explicit check good)
    # Removing 'i' from candidates if somehow in top (unlikely with diag=0 unless all are 0)
    candidates <- setdiff(ord, i)

    # Select Top K
    top_k_indices <- candidates[1:K]
    top_k_weights <- row_w[top_k_indices]

    neighbors[[i]] <- top_k_indices
    weights_list[[i]] <- top_k_weights
}

# 2.4 Create Spatial Weights Object (Row-Standardized)
# nb2listw with style="W" will row-normalize the weights we pass in 'glist'.
class(neighbors) <- "nb"
attr(neighbors, "region.id") <- as.character(1:N)
attr(neighbors, "call") <- match.call()

# Create listw object
W_norm <- nb2listw(neighbors, glist = weights_list, style = "W", zero.policy = TRUE)

# -------------------------------------------------------------------------
# 3. Diagnostics
# -------------------------------------------------------------------------
cat("\n--- Diagnostics ---\n")
# Check Row Sums (Should be 1)
w_mat_sparse <- listw2mat(W_norm)
row_sums <- rowSums(w_mat_sparse)
cat("Row Sums Summary (Expect all ~1):\n")
print(summary(row_sums))

# Check Non-Uniformity for a random node
set.seed(123)
sample_id <- sample(1:N, 1)
cat("\nWeights for Node", sample_id, "(Top 5):\n")
print(head(weights_list[[sample_id]] / sum(weights_list[[sample_id]]), 5)) # Normalized view

# -------------------------------------------------------------------------
# 4. Estimation
# -------------------------------------------------------------------------
cat("\n--- Estimating Models ---\n")

# Formula matches QMD
fmla_base <- y ~ age + educ + female +
    race_black + race_hispanic + race_asian + race_native + race_other +
    relig_catholic + relig_jewish + relig_none + relig_other

# Model 1: OLS Baseline
m1 <- lm(fmla_base, data = final_df)

# Model 2: Spatial Lag (Approximation via OLS for robustness/speed)
Wy <- lag.listw(W_norm, final_df$y)
final_df$Wy <- Wy # Add Wy for OLS
fmla_lag <- update(fmla_base, ~ . + Wy)
m2_ols <- lm(fmla_lag, data = final_df)

# Diagnostics for rho
cat("Estimated Rho (Wy coeff):", coef(m2_ols)["Wy"], "\n")

# -------------------------------------------------------------------------
# 5. Output Generation (Information only, or save if needed)
# -------------------------------------------------------------------------
# Compact Table (No SEs)
stargazer(m1, m2_ols,
    type = "text",
    title = "Democrat Scale Models (Replication V2)",
    column.labels = c("Baseline", "Spatial (Blau)"),
    dep.var.labels = "Democrat Thermometer",
    keep.stat = c("n", "rsq"),
    report = "vc*", # v=varname/coeff, c=column/model, *=stars. NO 's' (std.err) or 't'.
    omit.table.layout = "n" # compact
)

# Optional: Save Objects for QMD
dir.create("data/anes_2016_derived", showWarnings = FALSE)
saveRDS(W_norm, "data/anes_2016_derived/W_norm_k100.rds")
cat("\nSaved W_norm_k100.rds\n")
