# ==============================================================================
# 06_debug_values.R
# Debugging script to inspect Year coding and Education categories
# ==============================================================================

library(haven)
library(dplyr)

source("00_config.R")

# Load a sample of data
d85 <- read_dta(PATHS$GSS_1985, n_max = 500)
d04 <- read_dta(PATHS$GSS_2004, n_max = 500)

# 1. Check Year Coding
message("\n--- YEAR VARIABLE CHECK ---")
if ("year" %in% names(d85)) {
  message("1985 File - Year values:")
  print(table(d85$year))
}
if ("year" %in% names(d04)) {
  message("2004 File - Year values:")
  print(table(d04$year))
}

# 2. Check Alter Education Coding (educ1)
message("\n--- ALTER EDUCATION CHECK (educ1) ---")

check_educ <- function(df, label) {
  message(paste("Checking", label))
  if ("educ1" %in% names(df)) {
    message("Raw values table:")
    print(table(as.numeric(df$educ1)))
    
    message("Labels:")
    print(attr(df$educ1, "labels"))
  } else {
    message("Variable 'educ1' not found.")
  }
}

check_educ(d85, "1985 Data")
check_educ(d04, "2004 Data")

# 3. Check Ego Education (educ)
message("\n--- EGO EDUCATION CHECK (educ) ---")
check_ego_educ <- function(df, label) {
  message(paste("Checking", label))
  if ("educ" %in% names(df)) {
    message("Summary of Ego Educ (Years):")
    print(summary(as.numeric(df$educ)))
  }
}

check_ego_educ(d85, "1985 Data")
check_ego_educ(d04, "2004 Data")
