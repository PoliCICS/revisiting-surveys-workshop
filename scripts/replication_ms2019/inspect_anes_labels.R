# Script to inspect ANES 2016 labels to find relevant variables
# Usage: Rscript scripts/replication_ms2019/inspect_anes_labels.R

library(haven)
library(dplyr)
library(stringr)

# Load data (adjust path if necessary)
data_path <- "data/anes_timeseries_2016.dta"

if (!file.exists(data_path)) {
    stop("ANES 2016 file not found at: ", data_path)
}

cat("Loading ANES 2016 data...\n")
anes <- read_dta(data_path, n_max = 500) # Load first 500 rows for inspection is enough for labels

# Define patterns to search
patterns <- list(
    Democrat_Therm = c("Democratic Party", "thermometer"),
    Age = c("Age", "respondent"),
    Education = c("Education"),
    Gender = c("Gender", "Sex"),
    Race = c("Race", "Ethnicity"),
    Religion = c("Religion", "Church"),
    Weight = c("weight", "pre")
)

# Helper function to search labels
search_labels <- function(data, patterns) {
    found_vars <- list()

    for (cat_name in names(patterns)) {
        keywords <- patterns[[cat_name]]
        cat(paste0("\n--- Searching for: ", cat_name, " (Keywords: ", paste(keywords, collapse = ", "), ") ---\n"))

        # Iterate over all columns
        for (col in names(data)) {
            lbl <- attr(data[[col]], "label")
            if (is.null(lbl)) next

            # Check if ALL keywords are present (case insensitive)
            match <- TRUE
            for (kw in keywords) {
                if (!str_detect(lbl, regex(kw, ignore_case = TRUE))) {
                    match <- FALSE
                    break
                }
            }

            if (match) {
                cat(paste0(col, ": ", lbl, "\n"))
            }
        }
    }
}

search_labels(anes, patterns)
