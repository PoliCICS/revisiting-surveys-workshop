# ==============================================================================
# 01_data_prep.R
# Data Preparation for Smith, McPherson & Smith-Lovin (2014) Replication
# ==============================================================================

library(dplyr)
library(tidyr)
library(haven) # For reading .dta files if needed

source("00_config.R")

# ------------------------------------------------------------------------------
# Function: prepare_gss_data
# ------------------------------------------------------------------------------
# Purpose: Load and clean GSS data for a specific year.
# Inputs:
#   - file_path: Path to the GSS data file.
#   - year: The survey year (1985 or 2004).
# Output:
#   - A list containing:
#     - ego_data: Data frame of respondents (egos).
#     - dyad_data: Data frame of observed ego-alter ties (cases).
# ------------------------------------------------------------------------------

prepare_gss_data <- function(file_path, year) {
  
  message(paste("Processing GSS data for year:", year))
  
  # 1. Load Data
  # ----------------------------------------------------------------------------
  # Check file extension to determine loader
  if (grepl("\\.dta$", file_path)) {
    raw_data <- read_dta(file_path)
  } else if (grepl("\\.csv$", file_path)) {
    raw_data <- read.csv(file_path, stringsAsFactors = FALSE)
  } else {
    stop("Unsupported file format. Please use .dta or .csv")
  }
  
  # Filter by year if the file contains multiple years
  if (VAR_MAP$YEAR %in% names(raw_data)) {
    raw_data <- raw_data %>% filter(!!sym(VAR_MAP$YEAR) == year)
  }
  
  # 2. Select & Rename Ego Variables
  # ----------------------------------------------------------------------------
  # We extract ego attributes.
  # Note: Adjust variable selection based on actual GSS column names in your file.
  
  ego_vars <- c(VAR_MAP$ID, VAR_MAP$AGE, VAR_MAP$SEX, VAR_MAP$RACE, VAR_MAP$RELIG, VAR_MAP$EDUC)
  
  # Check if variables exist
  missing_vars <- setdiff(ego_vars, names(raw_data))
  if (length(missing_vars) > 0) {
    stop(paste("Missing ego variables in dataset:", paste(missing_vars, collapse = ", ")))
  }
  
  ego_df <- raw_data %>%
    select(all_of(ego_vars)) %>%
    rename(
      ego_id = !!sym(VAR_MAP$ID),
      ego_age = !!sym(VAR_MAP$AGE),
      ego_sex = !!sym(VAR_MAP$SEX),
      ego_race = !!sym(VAR_MAP$RACE),
      ego_relig = !!sym(VAR_MAP$RELIG),
      ego_educ = !!sym(VAR_MAP$EDUC)
    ) %>%
    mutate(year = year) %>%
    drop_na() # Remove egos with missing core attributes
  
  # 3. Process Alter Data (Reshape Wide to Long)
  # ----------------------------------------------------------------------------
  # GSS network data is typically wide: sex1, sex2, sex3...
  # We need to reshape this to a long format: ego_id, alter_rank, alter_sex...
  
  # Identify alter columns based on stems in VAR_MAP
  # This assumes standard GSS naming: sex1, sex2, etc.
  # You might need to adjust the regex or selection logic based on your specific file.
  
  # Helper to select and pivot
  # We assume up to 5 alters (GSS usually asks for up to 5)
  max_alters <- 5
  
  alter_long <- data.frame()
  
  for (i in 1:max_alters) {
    # Construct column names for the i-th alter
    col_age <- paste0(VAR_MAP$ALTER_AGE, i)
    col_sex <- paste0(VAR_MAP$ALTER_SEX, i)
    col_race <- paste0(VAR_MAP$ALTER_RACE, i)
    col_relig <- paste0(VAR_MAP$ALTER_RELIG, i)
    col_educ <- paste0(VAR_MAP$ALTER_EDUC, i)
    
    # Relationship Variables
    col_spouse <- paste0(VAR_MAP$REL_SPOUSE, i)
    col_parent <- paste0(VAR_MAP$REL_PARENT, i)
    col_sibling <- paste0(VAR_MAP$REL_SIBLING, i)
    col_child <- paste0(VAR_MAP$REL_CHILD, i)
    
    # Check if core columns exist
    current_cols <- c(col_age, col_sex, col_race, col_relig, col_educ)
    
    # Add relationship cols if they exist
    rel_cols <- c(col_spouse, col_parent, col_sibling, col_child)
    rel_cols_present <- rel_cols[rel_cols %in% names(raw_data)]
    
    if (all(current_cols %in% names(raw_data))) {
      
      # Select core + relationship vars
      temp <- raw_data %>%
        select(!!sym(VAR_MAP$ID), all_of(current_cols), all_of(rel_cols_present)) %>%
        rename(
          ego_id = !!sym(VAR_MAP$ID),
          alter_age = !!sym(col_age),
          alter_sex = !!sym(col_sex),
          alter_race = !!sym(col_race),
          alter_relig = !!sym(col_relig),
          alter_educ_cat = !!sym(col_educ)
        ) %>%
        mutate(alter_rank = i)
      
      # Calculate Kinship
      # If any of the kin columns are 1 (or mentioned), is_kin = 1
      # We assume GSS coding: 1 = Mentioned, NA/0 = Not mentioned
      # We need to be careful with NAs.
      
      # Helper to check if a column indicates kin
      check_kin <- function(col_name, df) {
        if (col_name %in% names(df)) {
          # Check if value is 1 (or TRUE)
          # GSS often uses 1 for "checked", NA or 0 for "not checked"
          return(coalesce(as.numeric(df[[col_name]]), 0) == 1)
        } else {
          return(FALSE)
        }
      }
      
      temp$is_spouse <- check_kin(col_spouse, temp)
      temp$is_parent <- check_kin(col_parent, temp)
      temp$is_sibling <- check_kin(col_sibling, temp)
      temp$is_child <- check_kin(col_child, temp)
      
      temp <- temp %>%
        mutate(is_kin = if_else(is_spouse | is_parent | is_sibling | is_child, 1, 0)) %>%
        select(-starts_with("is_spouse"), -starts_with("is_parent"), 
               -starts_with("is_sibling"), -starts_with("is_child"))
      
      # Remove original relationship columns to keep clean
      temp <- temp %>% select(-any_of(rel_cols_present))
      
      alter_long <- bind_rows(alter_long, temp)
    }
  }
  
  # 4. Clean & Recode Variables
  # ----------------------------------------------------------------------------
  
  # Join ego attributes to alter data to form dyads
  dyads <- alter_long %>%
    inner_join(ego_df, by = "ego_id")
  
  # Filter out empty alters (where no data was recorded)
  dyads <- dyads %>%
    filter(!is.na(alter_sex) & !is.na(alter_race)) # Basic check for existence
  
  # --- Recoding Logic (Crucial for Homophily) ---
  
  # RECODE ALTER RACE TO MATCH EGO RACE (1=White, 2=Black, 3=Other)
  # GSS Alter Race (race1..5) is usually: 1=Asian, 2=Black, 3=Hispanic, 4=White, 5=Other
  # We map: 4->1, 2->2, (1,3,5)->3
  
  dyads <- dyads %>%
    mutate(
      alter_race_orig = as.numeric(alter_race), # Keep original for debug
      alter_race = case_when(
        as.numeric(alter_race) == 4 ~ 1, # White
        as.numeric(alter_race) == 2 ~ 2, # Black
        as.numeric(alter_race) %in% c(1, 3, 5) ~ 3, # Other
        TRUE ~ NA_real_
      ),
      ego_race = as.numeric(ego_race) # Ensure numeric for comparison
    )

  # Education Mapping (Alter Categories -> Years)
  # Using the mapping defined in 00_config.R
  dyads <- dyads %>%
    mutate(
      alter_educ = as.numeric(as.character(factor(alter_educ_cat, 
                                                  levels = names(EDUC_MAPPING), 
                                                  labels = EDUC_MAPPING)))
    )
  
  # Handle missing education mapping
  # If alter_educ is NA but alter_educ_cat was not, we have a mapping issue.
  # For now, we drop NAs in key variables.
  
  dyads <- dyads %>%
    drop_na(ego_age, alter_age, ego_educ, alter_educ, 
            ego_sex, alter_sex, ego_race, alter_race, 
            ego_relig, alter_relig)
  
  # Define Kinship
  # is_kin is already calculated in the reshaping loop.
  # We ensure it exists.
  if (!"is_kin" %in% names(dyads)) {
    dyads$is_kin <- 0 # Fallback if calculation failed
  }
  
  # 5. Return Data
  # ----------------------------------------------------------------------------
  return(list(
    egos = ego_df,
    cases = dyads
  ))
}
