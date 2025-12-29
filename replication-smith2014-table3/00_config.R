# ==============================================================================
# 00_config.R
# Configuration for Smith, McPherson & Smith-Lovin (2014) Replication
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. File Paths
# ------------------------------------------------------------------------------
# Update these paths to point to your local GSS data extracts.
# The scripts assume data is in a format readable by haven::read_dta or read.csv
# (e.g., Stata .dta or .csv).

PATHS <- list(
  GSS_1985 = "../B - Surveys Data/GSS 1985/GSS1985.dta",
  GSS_2004 = "../B - Surveys Data/GSS 2004/GSS 2004 NORC.dta",
  OUTPUT_DIR = "output/"
)

# Create output directory if it doesn't exist
if (!dir.exists(PATHS$OUTPUT_DIR)) dir.create(PATHS$OUTPUT_DIR)

# ------------------------------------------------------------------------------
# 2. Variable Mappings
# ------------------------------------------------------------------------------
# Centralized mapping of GSS variable names to internal standardized names.
# This allows for easy updates if variable names change in different GSS releases.

VAR_MAP <- list(
  # ID Variables
  ID = "id",             # Respondent ID
  YEAR = "year",         # Survey Year
  
  # Ego Attributes
  AGE = "age",           # Age of respondent
  SEX = "sex",           # Sex of respondent
  RACE = "race",         # Race of respondent
  RELIG = "relig",       # Religion of respondent
  EDUC = "educ",         # Education (years) of respondent
  
  # Alter Attributes (Prefix 'mnt' often used in GSS for network module)
  # Note: GSS network data is usually wide (alter1, alter2, etc.)
  # We will reshape to long format. These are the base stems.
  ALTER_AGE = "age",     # e.g., age1, age2...
  ALTER_SEX = "sex",     # e.g., sex1, sex2...
  ALTER_RACE = "race",   # e.g., race1, race2...
  ALTER_RELIG = "relig", # e.g., relig1, relig2...
  ALTER_EDUC = "educ",   # e.g., educ1, educ2... (Categorical for alters)
  
  # Relationship Variables (Binary Flags)
  REL_SPOUSE = "spouse",
  REL_PARENT = "parent",
  REL_SIBLING = "sibling",
  REL_CHILD = "child",
  REL_COWORK = "cowork",
  REL_ADVISOR = "advisor",
  REL_FRIEND = "friend",
  REL_OTHER = "other",
  
  # Weights
  WTSS = "wtss"          # Weight variable (if needed, though case-control often unweighted or uses specific weights)
)

# ------------------------------------------------------------------------------
# 3. Constants & Parameters
# ------------------------------------------------------------------------------

CONSTANTS <- list(
  SEED = 12345,          # For reproducibility
  N_BOOTSTRAP = 1000,    # Number of bootstrap iterations for SEs
  CONTROL_RATIO = 1      # Number of controls per case (usually 1:1 or similar)
)

# ------------------------------------------------------------------------------
# 4. Education Mapping (Alter Categories to Years)
# ------------------------------------------------------------------------------
# Corrected Mapping based on GSS 1985/2004 Labels:
# 0: 1-6 years      -> 3.5
# 1: 7-9 years      -> 8
# 2: 10-12 years    -> 11 (Non-graduate)
# 3: HS grad        -> 12
# 4: Some college   -> 13.5
# 5: Associate deg  -> 14
# 6: Bachelor's     -> 16
# 7: Grad/Prof      -> 18

EDUC_MAPPING <- c(
  "0" = 3.5,
  "1" = 8,
  "2" = 11,
  "3" = 12,
  "4" = 13.5,
  "5" = 14,
  "6" = 16,
  "7" = 18
)

# ==============================================================================
# End of Config
# ==============================================================================
