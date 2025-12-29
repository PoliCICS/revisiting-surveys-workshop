# ==============================================================================
# 02_case_control_construction.R
# Case-Control Dyad Construction for Smith, McPherson & Smith-Lovin (2014)
# ==============================================================================

library(dplyr)
source("00_config.R")

# ------------------------------------------------------------------------------
# Function: generate_controls
# ------------------------------------------------------------------------------
# Purpose: Generate control dyads (non-ties) for a given set of egos.
# Inputs:
#   - ego_df: Data frame of respondents (egos) for a specific year.
#   - n_controls_per_case: Ratio of controls to generate (default 1).
# Output:
#   - control_dyads: Data frame of artificial dyads (ego-alter pairs).
# ------------------------------------------------------------------------------

generate_controls <- function(ego_df, n_controls_per_case = 1) {
  
  # We need to pair each ego with a random other individual from the population.
  # In this design, the "population" is the set of respondents (egos).
  # So, for each ego, we pick another ego to serve as the "alter".
  
  n_egos <- nrow(ego_df)
  target_n <- n_egos * n_controls_per_case
  
  # Create random pairings
  # We sample indices for 'alter' (who is actually another ego)
  # We ensure ego != alter
  
  control_list <- list()
  
  # Simple random sampling approach
  # Create a pool of potential alters (all egos)
  potential_alters <- ego_df
  
  # We iterate or vectorize. Vectorized is faster.
  # We'll create random pairs and filter out self-loops.
  
  # Sample egos (repeating if necessary to match case count, but here we just want 1:1 with respondents usually)
  # The prompt says: "Controls... Built by pairing respondents... into dyads"
  # Usually, we want the number of controls to match the number of cases (ties), 
  # OR match the number of respondents. 
  # Table 3 N(dyads) is ~1.1 million. N(respondents) is 3001.
  # This implies MANY dyads per respondent.
  # Wait, Table 3 says N(dyads) = 1,139,161 for Model 1/2.
  # N(respondents) = 3,001.
  # This is huge. 1.1M / 3000 ~ 380 dyads per respondent.
  # This suggests they might be pairing each ego with MANY potential alters, 
  # or using a very large control set.
  # OR, they are using all possible dyads? 3000^2 is 9 million.
  # 1.1M is about 1/9th of the full matrix.
  
  # RE-READING PROMPT: "Controls... Built by pairing respondents... into dyads that are assumed to be non-ties."
  # "This yields a large dyadic dataset... Rows = dyads (i, j)"
  
  # If I don't know the exact sampling ratio, I will default to a parameter.
  # However, to replicate the scale of Table 3, we might need a lot of controls.
  # But for a "replication pipeline skeleton", a 1:1 or 1:10 ratio is sufficient for testing.
  # I will stick to the CONSTANTS$CONTROL_RATIO from config.
  
  # Let's assume we generate 'k' controls per ego.
  k <- CONSTANTS$CONTROL_RATIO
  
  # Replicate egos k times
  egos_expanded <- ego_df[rep(1:n_egos, each = k), ]
  
  # Sample alters randomly
  # We sample indices from 1 to n_egos
  alter_indices <- sample(1:n_egos, nrow(egos_expanded), replace = TRUE)
  
  # Bind together
  controls <- egos_expanded
  
  # Add "Alter" attributes (which are actually the attributes of the sampled ego)
  # We rename the sampled ego's attributes to 'alter_...'
  sampled_alters <- ego_df[alter_indices, ]
  
  controls$alter_id <- sampled_alters$ego_id
  controls$alter_age <- sampled_alters$ego_age
  controls$alter_sex <- sampled_alters$ego_sex
  controls$alter_race <- sampled_alters$ego_race
  controls$alter_relig <- sampled_alters$ego_relig
  controls$alter_educ <- sampled_alters$ego_educ # Note: Ego educ is already in years
  
  # Remove self-loops (ego paired with themselves)
  controls <- controls %>% filter(ego_id != alter_id)
  
  # Mark as controls
  controls$tie <- 0
  controls$is_kin <- 0 # Controls are assumed non-kin (or generic)
  
  return(controls)
}

# ------------------------------------------------------------------------------
# Function: calculate_predictors
# ------------------------------------------------------------------------------
# Purpose: Calculate homophily predictors (distances) for a dyadic dataset.
# Inputs:
#   - dyad_df: Data frame with ego_... and alter_... columns.
# Output:
#   - dyad_df: The same data frame with added predictor columns.
# ------------------------------------------------------------------------------

calculate_predictors <- function(dyad_df) {
  
  dyad_df <- dyad_df %>%
    mutate(
      # 1. Binary Differences (1 = Different, 0 = Same)
      diff_sex = if_else(ego_sex != alter_sex, 1, 0),
      diff_race = if_else(ego_race != alter_race, 1, 0),
      diff_relig = if_else(ego_relig != alter_relig, 1, 0),
      
      # 2. Continuous Differences (Absolute distance)
      age_diff = abs(ego_age - alter_age),
      
      # Education Difference
      # Note: ego_educ is years, alter_educ is years (mapped)
      edu_diff = abs(ego_educ - alter_educ)
    )
  
  return(dyad_df)
}

# ------------------------------------------------------------------------------
# Function: build_analysis_dataset
# ------------------------------------------------------------------------------
# Purpose: Combine cases and controls, and prepare for modeling.
# ------------------------------------------------------------------------------

build_analysis_dataset <- function(cases, controls) {
  
  # Ensure columns match
  common_cols <- intersect(names(cases), names(controls))
  
  # Add 'tie' variable to cases if not present
  if (!"tie" %in% names(cases)) cases$tie <- 1
  
  # Select common columns
  cases_sub <- cases %>% select(any_of(c(common_cols, "tie", "year", "is_kin")))
  controls_sub <- controls %>% select(any_of(c(common_cols, "tie", "year", "is_kin")))
  
  # Combine
  full_data <- bind_rows(cases_sub, controls_sub)
  
  # Calculate predictors
  full_data <- calculate_predictors(full_data)
  
  return(full_data)
}
