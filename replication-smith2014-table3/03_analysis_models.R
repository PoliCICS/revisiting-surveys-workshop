# ==============================================================================
# 03_analysis_models.R
# Model Estimation and Bootstrapping for Smith, McPherson & Smith-Lovin (2014)
# ==============================================================================

library(dplyr)
library(broom) # For tidy model output

# ------------------------------------------------------------------------------
# Model Formulas
# ------------------------------------------------------------------------------

FORMULAS <- list(
  # Model 1: All ties, main effects
  model1 = as.formula("tie ~ diff_race + diff_relig + diff_sex + age_diff + edu_diff + year"),
  
  # Model 2: All ties, interactions with Year
  model2 = as.formula("tie ~ diff_race + diff_relig + diff_sex + age_diff + edu_diff + year + 
                      diff_race:year + diff_relig:year + diff_sex:year + age_diff:year + edu_diff:year"),
  
  # Model 3: Non-kin ties, main effects (Same formula as 1, different data subset)
  model3 = as.formula("tie ~ diff_race + diff_relig + diff_sex + age_diff + edu_diff + year"),
  
  # Model 4: Non-kin ties, interactions (Same formula as 2, different data subset)
  model4 = as.formula("tie ~ diff_race + diff_relig + diff_sex + age_diff + edu_diff + year + 
                      diff_race:year + diff_relig:year + diff_sex:year + age_diff:year + edu_diff:year")
)

# ------------------------------------------------------------------------------
# Function: fit_logistic_models
# ------------------------------------------------------------------------------
# Purpose: Fit Models 1-4 on a given dataset.
# Inputs:
#   - data: The full case-control dyadic dataset.
# Output:
#   - A list of fitted model objects (or coefficients).
# ------------------------------------------------------------------------------

fit_logistic_models <- function(data) {
  
  # Prepare subsets
  # Models 1 & 2 use ALL ties (cases) + controls
  # Models 3 & 4 use NON-KIN ties (cases) + controls
  # Note: Controls are usually assumed non-kin or generic. 
  # If 'is_kin' is NA for controls, we assume 0.
  
  data_all <- data
  data_nonkin <- data %>% filter(is_kin == 0 | tie == 0) # Keep non-kin cases AND all controls
  
  # Fit Models
  m1 <- glm(FORMULAS$model1, data = data_all, family = binomial(link = "logit"))
  m2 <- glm(FORMULAS$model2, data = data_all, family = binomial(link = "logit"))
  m3 <- glm(FORMULAS$model3, data = data_nonkin, family = binomial(link = "logit"))
  m4 <- glm(FORMULAS$model4, data = data_nonkin, family = binomial(link = "logit"))
  
  return(list(m1 = m1, m2 = m2, m3 = m3, m4 = m4))
}

# ------------------------------------------------------------------------------
# Function: run_bootstrap
# ------------------------------------------------------------------------------
# Purpose: Estimate Bootstrap Standard Errors.
# Inputs:
#   - full_data: The complete dyadic dataset (cases + controls).
#   - n_boot: Number of bootstrap iterations.
# Output:
#   - A data frame of bootstrap SEs for each term in each model.
# ------------------------------------------------------------------------------

run_bootstrap <- function(full_data, n_boot = 100) {
  
  message(paste("Starting Bootstrap with", n_boot, "iterations..."))
  
  # Storage for coefficients
  # We will store results as a list of data frames
  boot_results <- list()
  
  # Identify unique respondents (clusters)
  # We must resample respondents, then grab all their dyads.
  respondents <- unique(full_data$ego_id)
  n_respondents <- length(respondents)
  
  for (i in 1:n_boot) {
    if (i %% 10 == 0) message(paste("Bootstrap iteration:", i))
    
    # 1. Resample Respondents
    resampled_ids <- sample(respondents, n_respondents, replace = TRUE)
    
    # 2. Reconstruct Dataset
    # Since we have 'resampled_ids' which may contain duplicates, 
    # we can't just use 'filter'. We need to replicate rows for duplicate IDs.
    
    # Create a mapping table: resampled_id -> original_id
    # (Actually, we just need to pull rows for each ID in the sample)
    
    # Efficient way:
    # Split data by ego_id (can be slow if many groups)
    # Or use join.
    
    # Let's make a table of the sampled IDs
    sample_df <- data.frame(ego_id = resampled_ids)
    
    # Inner join duplicates rows if ego_id appears multiple times in sample_df
    boot_data <- full_data %>%
      inner_join(sample_df, by = "ego_id", relationship = "many-to-many")
    
    # 3. Fit Models
    models <- fit_logistic_models(boot_data)
    
    # 4. Extract Coefficients
    # We extract tidy coefficients for each model
    coefs <- bind_rows(
      tidy(models$m1) %>% mutate(model = "Model 1"),
      tidy(models$m2) %>% mutate(model = "Model 2"),
      tidy(models$m3) %>% mutate(model = "Model 3"),
      tidy(models$m4) %>% mutate(model = "Model 4")
    ) %>%
      mutate(boot_iter = i)
    
    boot_results[[i]] <- coefs
  }
  
  # Combine all results
  all_boot_coefs <- bind_rows(boot_results)
  
  # Calculate SD of coefficients (Bootstrap SE)
  boot_se <- all_boot_coefs %>%
    group_by(model, term) %>%
    summarise(
      std.error.boot = sd(estimate, na.rm = TRUE),
      estimate.mean = mean(estimate, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(boot_se)
}
