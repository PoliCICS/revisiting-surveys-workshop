library(haven)
library(dplyr)
library(stringr)

data_path <- "data/anes_timeseries_2016.dta"
anes <- read_dta(data_path, n_max = 500)

patterns <- list(
    # Gender often "R gender" or "What is your sex?"
    Gender = c("gender", "sex", "male", "female"),

    # Race often "R self-ident" or specific groups
    Race = c("race", "ethnicity", "origin", "white", "black", "hispanic"),

    # Religion often "church", "service", "faith"
    Religion = c("religion", "church", "jewish", "catholic", "protestant", "bible")
)

search_labels <- function(data, patterns) {
    for (cat_name in names(patterns)) {
        keywords <- patterns[[cat_name]]
        cat(paste0("\n--- Searching for: ", cat_name, " ---\n"))

        for (col in names(data)) {
            lbl <- attr(data[[col]], "label")
            if (is.null(lbl)) next

            # Use OR logic for broader search
            if (str_detect(lbl, regex(paste(keywords, collapse = "|"), ignore_case = TRUE))) {
                cat(paste0(col, ": ", lbl, "\n"))
            }
        }
    }
}

search_labels(anes, patterns)
