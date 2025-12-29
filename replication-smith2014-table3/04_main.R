# ==============================================================================
# 04_main.R
# Master Script: Replication of Smith, McPherson & Smith-Lovin (2014), Table 3
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Setup & Configuration
# ------------------------------------------------------------------------------
rm(list = ls()) # Clear environment
source("00_config.R")
source("01_data_prep.R")
source("02_case_control_construction.R")
source("03_analysis_models.R")

library(dplyr)
library(readr)

# Set Seed
set.seed(CONSTANTS$SEED)

message("========================================================")
message("Starting Replication Pipeline")
message("========================================================")

# ------------------------------------------------------------------------------
# 2. Data Loading & Preparation
# ------------------------------------------------------------------------------

# NOTE: Ensure paths in 00_config.R are correct before running.
# If files are missing, this section will fail.

# Check if files exist
if (!file.exists(PATHS$GSS_1985) || !file.exists(PATHS$GSS_2004)) {
  warning("GSS data files not found at configured paths. Please update 00_config.R.")
  # For demonstration purposes, we might stop here or proceed if testing with dummy data.
  # stop("Data files missing.")
}

# Load 1985 Data
data_1985 <- prepare_gss_data(PATHS$GSS_1985, year = 1985)
egos_1985 <- data_1985$egos
cases_1985 <- data_1985$cases

# Load 2004 Data
data_2004 <- prepare_gss_data(PATHS$GSS_2004, year = 2004)
egos_2004 <- data_2004$egos
cases_2004 <- data_2004$cases
# ------------------------------------------------------------------------------
# 3. Control Generation
# ------------------------------------------------------------------------------
message("Generating Control Dyads...")

controls_1985 <- generate_controls(egos_1985, n_controls_per_case = CONSTANTS$CONTROL_RATIO)
controls_2004 <- generate_controls(egos_2004, n_controls_per_case = CONSTANTS$CONTROL_RATIO)

# ------------------------------------------------------------------------------
# 4. Build Analysis Dataset
# ------------------------------------------------------------------------------
message("Building Analysis Dataset...")

full_data_1985 <- build_analysis_dataset(cases_1985, controls_1985)
full_data_2004 <- build_analysis_dataset(cases_2004, controls_2004)

# Combine years
full_data <- bind_rows(full_data_1985, full_data_2004)

# RECODE YEAR TO 0/1 (Critical for Interactions)
# 1985 -> 0
# 2004 -> 1
full_data <- full_data %>%
  mutate(year = if_else(year == 2004, 1, 0))

# Validation Checks
message("Dataset Summary:")
print(table(full_data$year, full_data$tie))

# ------------------------------------------------------------------------------
# 5. Model Estimation (Point Estimates)
# ------------------------------------------------------------------------------
message("Fitting Models 1-4 (Point Estimates)...")

models <- fit_logistic_models(full_data)

# Extract Point Estimates
results_point <- bind_rows(
  tidy(models$m1) %>% mutate(model = "Model 1"),
  tidy(models$m2) %>% mutate(model = "Model 2"),
  tidy(models$m3) %>% mutate(model = "Model 3"),
  tidy(models$m4) %>% mutate(model = "Model 4")
) %>%
  select(model, term, estimate)

# ------------------------------------------------------------------------------
# 6. Bootstrap Standard Errors
# ------------------------------------------------------------------------------
message(paste("Running Bootstrap SE Estimation (", CONSTANTS$N_BOOTSTRAP, " iterations)..."))
# Note: This can take a long time.
# For testing, reduce N_BOOTSTRAP in 00_config.R

boot_se <- run_bootstrap(full_data, n_boot = CONSTANTS$N_BOOTSTRAP)

# ------------------------------------------------------------------------------
# 7. Final Output Generation
# ------------------------------------------------------------------------------
message("Compiling Final Table...")

# Merge Point Estimates with Bootstrap SEs
final_table <- results_point %>%
  left_join(boot_se, by = c("model", "term")) %>%
  mutate(
    t_stat = estimate / std.error.boot,
    p_val = 2 * (1 - pnorm(abs(t_stat))), # Approx p-value
    significance = case_when(
      p_val < 0.001 ~ "***",
      p_val < 0.01 ~ "**",
      p_val < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>%
  select(model, term, estimate, std.error.boot, significance) %>%
  arrange(model, term)

print(final_table)

# Save to CSV
write_csv(final_table, file.path(PATHS$OUTPUT_DIR, "replication_table3.csv"))

message("Pipeline Completed Successfully.")
message(paste("Results saved to:", file.path(PATHS$OUTPUT_DIR, "replication_table3.csv")))
