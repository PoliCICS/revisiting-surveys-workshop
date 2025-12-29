# Replication of Smith, McPherson & Smith-Lovin (2014) - Table 3

This project aims to replicate the case-control logistic regression models (Models 1-4) reported in Table 3 of:
**Smith, McPherson & Smith-Lovin (2014), "Social Distance in the United States: Sex, Race, Religion, Age, and Education Homophily among Confidants, 1985–2004".**

## Project Structure

- **00_config.R**: Configuration file containing file paths, variable mappings, and global constants.
- **01_data_prep.R**: Functions to load and clean GSS ego-alter data.
- **02_case_control_construction.R**: Functions to construct the case-control dyadic dataset (pairing egos with observed alters and random controls).
- **03_analysis_models.R**: Functions to fit logistic regression models and perform bootstrap standard error estimation.
- **04_main.R**: Master script that orchestrates the entire pipeline.

## Methodology

The analysis uses a **case-control design**:
- **Cases**: Observed ego-alter confidant ties.
- **Controls**: Artificially constructed non-ties formed by pairing respondents with random other respondents (or their alters) to simulate the pool of potential contacts.

The models estimate the probability of a tie based on homophily (similarity) in:
- Race
- Religion
- Sex
- Age
- Education

## Usage

1. Ensure GSS data files are available (paths configured in `00_config.R`).
2. Run `04_main.R` to execute the full pipeline.
