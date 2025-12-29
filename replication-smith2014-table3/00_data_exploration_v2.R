# ==============================================================================
# 00_data_exploration_v2.R
# Deep dive into Relationship Variables for GSS 2004
# ==============================================================================

library(haven)
library(dplyr)
library(stringr)

path_2004 <- "../B - Surveys Data/GSS 2004/GSS 2004 NORC.dta"

inspect_relationships_2004 <- function() {
  message(paste("Inspecting:", path_2004))
  
  if (!file.exists(path_2004)) {
    message("FILE NOT FOUND!")
    return(NULL)
  }
  
  data <- read_dta(path_2004, n_max = 100)
  cols <- names(data)
  
  message("\n--- Relationship Variables Search (2004) ---")
  # Common GSS network relationship codes
  patterns <- c("spouse", "parent", "sibling", "child", "family", "kin", "partner", "cowork", "neighbor", "member", "advisor", "friend", "other")
  
  for (pat in patterns) {
    # Look for pattern followed by a number (e.g., spouse1)
    matches <- cols[str_detect(cols, paste0("^", pat, "[0-9]"))]
    if (length(matches) > 0) {
      message(paste("Found '", pat, "': ", paste(matches, collapse = ", "), sep = ""))
    }
  }
  
  # Also check for 'close' again just in case
  matches_close <- cols[str_detect(cols, "^close[0-9]")]
  if (length(matches_close) > 0) {
      message(paste("Found 'close': ", paste(matches_close, collapse = ", "), sep = ""))
  }
}

inspect_relationships_2004()
