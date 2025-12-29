# ==============================================================================
# 05_diagnostics.R
# Diagnostic checks for GSS variables and Model Results
# ==============================================================================

library(haven)
library(dplyr)

source("00_config.R")

# Load raw data again to check labels
d85 <- read_dta(PATHS$GSS_1985, n_max = 500)
d04 <- read_dta(PATHS$GSS_2004, n_max = 500)

check_labels <- function(df, year) {
  message(paste("\n--- Checking Labels for Year:", year, "---"))
  
  # Ego Race
  if ("race" %in% names(df)) {
    message("Ego Race (race):")
    print(attr(df$race, "labels"))
  }
  
  # Alter Race
  if ("race1" %in% names(df)) {
    message("Alter Race (race1):")
    print(attr(df$race1, "labels"))
  }
  
  # Cross-tab if possible (first 500 rows)
  if ("race" %in% names(df) & "race1" %in% names(df)) {
    message("Crosstab Ego Race vs Alter Race 1 (Raw Codes):")
    print(table(as.numeric(df$race), as.numeric(df$race1)))
  }
}

check_labels(d85, 1985)
check_labels(d04, 2004)

# Check loaded analysis data (if available)
if (file.exists(file.path(PATHS$OUTPUT_DIR, "replication_table3.csv"))) {
  res <- read.csv(file.path(PATHS$OUTPUT_DIR, "replication_table3.csv"))
  message("\n--- Current Results ---")
  print(res)
}
