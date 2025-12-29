# ==============================================================================
# 00_data_exploration.R
# Exploratory Data Analysis for GSS 1985 and 2004 Files
# Purpose: Identify variable names for Ego and Alter attributes.
# ==============================================================================

library(haven)
library(dplyr)
library(stringr)

# Paths provided by user/context
path_1985 <- "../B - Surveys Data/GSS 1985/GSS1985.dta"
path_2004 <- "../B - Surveys Data/GSS 2004/GSS 2004 NORC.dta"

# Function to inspect a dataset
inspect_gss <- function(file_path, label) {
  message(paste("\n=========================================="))
  message(paste("Inspecting:", label))
  message(paste("Path:", file_path))
  message(paste("=========================================="))
  
  if (!file.exists(file_path)) {
    message("FILE NOT FOUND!")
    return(NULL)
  }
  
  # Read only top 100 rows to save time/memory if possible, 
  # but read_dta reads full file usually. We'll read full and subset.
  data <- read_dta(file_path, n_max = 100)
  
  cols <- names(data)
  message(paste("Total Columns:", length(cols)))
  
  # Check for Year
  if ("year" %in% cols) {
    years <- unique(data$year)
    message(paste("Years present in first 100 rows:", paste(years, collapse = ", ")))
  } else {
    message("Variable 'year' NOT found.")
  }
  
  # Helper to find vars
  find_vars <- function(pattern) {
    matches <- cols[str_detect(cols, pattern)]
    if (length(matches) > 0) {
      message(paste("Found matches for '", pattern, "': ", paste(head(matches, 20), collapse = ", "), sep = ""))
      if (length(matches) > 20) message("... (truncated)")
    } else {
      message(paste("No matches for '", pattern, "'", sep = ""))
    }
  }
  
  # 1. Ego Attributes
  message("\n--- Ego Attributes ---")
  find_vars("^age$")
  find_vars("^sex$")
  find_vars("^race")
  find_vars("^educ$")
  find_vars("^relig$")
  find_vars("^id$")
  
  # 2. Alter Attributes (Network Module)
  message("\n--- Alter Attributes (Network Module) ---")
  # Look for numbered variables like sex1, age1, etc.
  find_vars("^sex[0-9]")
  find_vars("^race[0-9]")
  find_vars("^educ[0-9]")
  find_vars("^age[0-9]")
  find_vars("^relig[0-9]")
  find_vars("^close[0-9]") # Relationship
  find_vars("^kin[0-9]")
  find_vars("^friend[0-9]")
  find_vars("^talk[0-9]")
  find_vars("^spouse[0-9]")
  find_vars("^parent[0-9]")
  find_vars("^sibling[0-9]")
  find_vars("^child[0-9]")
  
  # 3. Check for specific GSS network vars if standard names fail
  # Sometimes they are named differently in different years
  
  return(invisible(NULL))
}

# Run Inspection
inspect_gss(path_1985, "GSS 1985 File")
inspect_gss(path_2004, "GSS 2004 File")
