# Purpose: attach packages and define every constant shared by the scripts.
# Needs: nothing. Produces: attached packages and named constants.
# The data are located only through the RUSH_DATA_DIR environment variable,
# set in .Renviron by the person authorized to run the analysis.

## ---- load-packages
library(tidyverse)  # dplyr, tidyr, stringr, purrr
library(readxl)     # the de-identified Excel workbooks
library(survival)   # clogit(): conditional logistic regression
library(lme4)       # glmer(): random-effects logistic regression
library(gtsummary)  # Tables 1 to 4 (R/tables.R)

## ---- define-data-files
# The original analysis was dated 15 January 2015; the workbook names carry
# that date, as in the Stata file's $DateOfAnalysis global.
data_dir <- Sys.getenv("RUSH_DATA_DIR")
analysis_date <- "1-15-2015"
years_since_dx_file <- str_glue("Deidentified Years_After_DX {analysis_date}.xlsx")
rush_data_file <- str_glue("Deidentified RUSH data {analysis_date}.xlsx")
study_cohort_sheet <- "Study Cohort"
control_cohort_sheet <- "Controls"
# read_excel() guesses each column's type from its first 1,000 rows by
# default; a text column that is empty there (a viral load result, say)
# could be read as logical and lose its later values. Guessing from every
# row, as Stata's import excel types the whole column, avoids that.
excel_guess_rows <- 1e6

## ---- define-expected-columns
# The column names the Stata file reads, exactly as written there (note
# EngageInCare_Before but EngagedInCare_After). The import stops if any is
# missing. Their meaning comes from the article's Methods
# (analysis-plan.md section 7), not from the names.
years_since_dx_columns <- c(
  "PatientNumber",
  "Years_between_DX_and_Index_Visit",
  "Category"
)
cohort_columns <- c(
  "PatientNumber", "Race", "Gender", "AgeGroup", "IndexTestYear",
  "IndexTest_Place",
  "EngageInCare_Before", "EngagedInCare_After",
  "RetainedInCare_Before", "RetainedInCare_After",
  "VL_Before", "VL_After",
  "EngageInCare_Before_Place", "EngageInCare_After_Place",
  "RetainedInCare_Before_Place", "RetainedInCare_After_Place"
)
# The control sheet: the same columns, except that the original code never
# reads the year of the visit there
control_columns <- setdiff(cohort_columns, "IndexTestYear")
# Where each outcome was recorded; the original code drops these unused
place_columns <- c(
  "IndexTest_Place",
  "EngageInCare_Before_Place", "EngageInCare_After_Place",
  "RetainedInCare_Before_Place", "RetainedInCare_After_Place"
)

## ---- define-categories
# Years from HIV diagnosis to the index visit, in the order of the original
# code's labels (1 = "<=5" ... 5 = ">20"). The first is the reference group.
years_since_dx_levels <- c("<=5", "6-10", "11-15", "16-20", ">20")

# Viral load results as recorded in the workbook (analysis-plan.md 7.3)
vl_suppressed_value <- "Suppressed"
vl_not_suppressed_value <- "Not Suppressed"

# Four race and ethnicity groups (analysis-plan.md 8.4). The original code
# grouped Stata's encode() codes, which number the recorded race values in
# alphabetical order within each workbook sheet, so the same code can stand
# for different values in the two sheets. The codes below are those of the
# original code for each sheet; R/02_build_long_data.R reproduces encode()'s
# numbering and prints the code-to-value table so an authorized person can
# check them. Still to check against the data: the recorded value behind
# each code (issue #4).
race_group_levels <- c("Black", "Hispanic", "White", "Asian/Other/Unknown")
race_group_codes_study <- list(
  Black = 2,
  Hispanic = 3,
  White = c(4, 7),
  `Asian/Other/Unknown` = c(1, 5, 6)
)
race_group_codes_control <- list(
  Black = 2,
  Hispanic = 3,
  White = 5,
  `Asian/Other/Unknown` = c(1, 4)
)
# The original code's AfricanAmerican indicator is race code 2 in both sheets
african_american_code <- 2

## ---- define-outcomes
# Each outcome's variable name and the label used in output. The two viral
# load outcomes differ only in how a missing result is treated
# (analysis-plan.md 7.3): primary, missing counts as not suppressed.
outcomes <- c(
  vl_suppressed_missing_fail = "Viral suppression (missing = not suppressed)",
  vl_suppressed = "Viral suppression (missing excluded)",
  engaged_in_care = "Engaged in care",
  retained_in_care = "Retained in care"
)

## ---- define-subgroups
# Characteristics whose interaction with the visit the original code tested
# in the RUSH-era cohort (analysis-plan.md 8.4), in the original code's order
subgroups_rush <- c(
  index_test_year = "Year of index visit",
  years_since_dx_group = "Years since diagnosis",
  gender = "Sex",
  age_group = "Age group",
  race = "Race and ethnicity (as recorded)",
  race_group = "Race and ethnicity (four groups)",
  african_american = "African American"
)
# Characteristics in the three-way era comparisons (analysis-plan.md 8.6).
# Year of visit is left out: it cannot overlap between the two eras.
subgroups_era <- c(
  gender = "Sex",
  age_group = "Age group",
  race_group = "Race and ethnicity (four groups)"
)

## ---- define-skipped-models
# Models the original code did not run, each commented out there because the
# fit failed ("regression fails"; "data sparseness"). They are skipped here
# too, so the R port fits the same set of models (quarto/differences.qmd).
skipped_rush_subgroup_models <- tribble(
  ~outcome,        ~subgroup,
  "vl_suppressed", "race"
)
skipped_era_subgroup_models_all <- tribble(
  ~outcome,                     ~subgroup,
  "vl_suppressed_missing_fail", "race_group",
  "retained_in_care",           "race_group"
)

## ---- define-estimation-constants
confidence_level <- 0.95
# Random-effects models: adaptive Gauss-Hermite quadrature with 12 points,
# Stata's default for xtlogit
quadrature_points <- 12
# Patients with both a RUSH-era and a pre-RUSH visit have four rows in the
# long data: before and after each visit
rows_per_patient_both_eras <- 4

## ---- define-table-constants
# Tables 1 to 4 (R/tables.R): the characteristics, in the article's row
# order, with their headings; the three outcomes of Tables 2 to 4; and the
# category labels the article prints where they differ from the recorded
# ones. Years since diagnosis start at 1 because the cohort excluded
# patients diagnosed less than a year before the visit.
table_characteristics <- c(
  gender = "Sex",
  race_group = "Race",
  age_group = "Age group",
  index_test_year = "Year of visit",
  years_since_dx_group = "Years since diagnosis"
)
table_outcomes <- c(
  engaged_in_care = "**Engagement in care**",
  retained_in_care = "**Retention in care**",
  vl_suppressed_missing_fail = "**Virologic suppression**"
)
table_numbers <- c(
  engaged_in_care = "table_2",
  retained_in_care = "table_3",
  vl_suppressed_missing_fail = "table_4"
)
table_level_labels <- list(
  gender = character(),
  race_group = c(`Asian/Other/Unknown` = "Other"),
  age_group = character(),
  index_test_year = character(),
  years_since_dx_group = c(
    `<=5` = "1–5", `6-10` = "6–10", `11-15` = "11–15", `16-20` = "16–20",
    `>20` = "21+"
  )
)
tables_dir <- "tables"
