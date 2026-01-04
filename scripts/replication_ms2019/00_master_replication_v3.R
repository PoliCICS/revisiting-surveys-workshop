# -------------------------------------------------------------------------
# Master Replication V3: Homophily Strength (Smith & McPherson 2014)
# -------------------------------------------------------------------------
# Goal: Replicate Table 3 logic and confirm coefficients for 2004 imputation.
# Use this script to validate the 'betas' vector for the network module.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, broom, stargazer)

# 1. Load Clean Data (Same as QMD)
# -------------------------------------------------------------------------
cat("Loading pre-cleaned GSS data...\n")
FILE_1985 <- "data/clean_gss_1985.rds"
FILE_2004 <- "data/clean_gss_2004.rds"

if (!file.exists(FILE_1985)) stop("Missing 1985 data")
if (!file.exists(FILE_2004)) stop("Missing 2004 data")

egos_1985 <- readRDS(FILE_1985)
egos_unique_1985 <- egos_1985 %>%
    select(ego_id, ego_age, ego_sex, ego_race, ego_relig, ego_educ, year) %>%
    distinct()

egos_2004 <- readRDS(FILE_2004)
egos_unique_2004 <- egos_2004 %>%
    select(ego_id, ego_age, ego_sex, ego_race, ego_relig, ego_educ, year) %>%
    distinct()

# Cases (Tie = 1)
cases_1985 <- egos_1985
cases_2004 <- egos_2004
all_cases <- bind_rows(cases_1985, cases_2004)
all_cases$tie <- 1
all_cases$is_kin <- 0

# 2. Generate Controls (Tie = 0)
# -------------------------------------------------------------------------
# Helper function (Same as QMD)
generate_controls <- function(ego_df, n_controls_per_case) {
    n_egos <- nrow(ego_df)
    controls <- ego_df[rep(1:n_egos, each = n_controls_per_case), ]

    # Sample alters with replacement
    alter_indices <- sample(1:n_egos, nrow(controls), replace = TRUE)
    sampled_alters <- ego_df[alter_indices, ]

    # Assign alter attributes
    controls$alter_id <- sampled_alters$ego_id
    controls$alter_age <- sampled_alters$ego_age
    controls$alter_sex <- sampled_alters$ego_sex
    controls$alter_race <- sampled_alters$ego_race
    controls$alter_relig <- sampled_alters$ego_relig
    controls$alter_educ <- sampled_alters$ego_educ

    # Remove accidental self-ties
    controls <- controls %>% filter(ego_id != alter_id)
    controls$tie <- 0
    controls$is_kin <- 0
    return(controls)
}

set.seed(123) # Reproducibility
controls_1985 <- generate_controls(egos_unique_1985, n_controls_per_case = 10)
controls_2004 <- generate_controls(egos_unique_2004, n_controls_per_case = 10)
all_controls <- bind_rows(controls_1985, controls_2004)

# 3. Create Analysis Dataset (Distances)
# -------------------------------------------------------------------------
analysis_data <- bind_rows(
    all_cases %>% select(any_of(names(all_controls))),
    all_controls
)

analysis_data <- analysis_data %>%
    mutate(
        # Mismatch Indicators
        diff_sex = if_else(ego_sex != alter_sex, 1, 0),
        diff_race = if_else(ego_race != alter_race, 1, 0),
        diff_relig = if_else(ego_relig != alter_relig, 1, 0),
        # Absolute Differences
        age_diff = abs(ego_age - alter_age),
        edu_diff = abs(ego_educ - alter_educ)
    )

cat("Analysis Dataset: N =", nrow(analysis_data), "\n")

# 4. Estimate Models
# -------------------------------------------------------------------------

# Model 1: Main Effects
m1 <- glm(tie ~ diff_race + diff_relig + diff_sex + age_diff + edu_diff + factor(year),
    data = analysis_data,
    family = binomial(link = "logit")
)

# Model 2: Interaction with Year
m2 <- glm(tie ~ (diff_race + diff_relig + diff_sex + age_diff + edu_diff) * factor(year),
    data = analysis_data,
    family = binomial(link = "logit")
)

# 5. Report Coefficients (Validation)
# -------------------------------------------------------------------------
stargazer(m1, m2,
    type = "text",
    title = "Homophily Strength Models (Validation)",
    star.cutoffs = c(0.05, 0.01, 0.001),
    keep.stat = c("n")
)

# Calculate 2004 Betas (Beta + Delta)
c2 <- coef(m2)
cat("\n--- Calculated 2004 Betas (Baseline + Interaction) ---\n")
beta_race <- c2["diff_race"] + c2["diff_race:factor(year)2004"]
beta_relig <- c2["diff_relig"] + c2["diff_relig:factor(year)2004"]
beta_sex <- c2["diff_sex"] + c2["diff_sex:factor(year)2004"]
beta_age <- c2["age_diff"] + c2["age_diff:factor(year)2004"]
beta_educ <- c2["edu_diff"] + c2["edu_diff:factor(year)2004"]

cat("Race:", beta_race, "\n")
cat("Relig:", beta_relig, "\n")
cat("Sex:", beta_sex, "\n")
cat("Age:", beta_age, "\n")
cat("Educ:", beta_educ, "\n")
