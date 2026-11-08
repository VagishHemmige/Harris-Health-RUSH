# Purpose: read the de-identified workbooks, add years since diagnosis to the
# RUSH-era cohort, and turn the text categories into factors numbered as the
# original code's Stata encode numbered them.
# Needs: data_dir, the file, sheet and column names, years_since_dx_levels
# and place_columns (R/setup.R); check_columns() and clean_cohort()
# (R/functions.R); RUSH_DATA_DIR set in .Renviron.
# Produces: study_wide (one row per patient in the RUSH-era cohort) and
# control_wide (one row per patient with a pre-RUSH visit).

## ---- check-data-folder
if (data_dir == "" || !dir.exists(data_dir)) {
  stop("Set RUSH_DATA_DIR in .Renviron to the folder holding the workbooks.")
}

## ---- read-years-since-diagnosis
# One row per patient in the RUSH-era cohort: years from HIV diagnosis to
# the index visit, as a number and in five groups. A group text other than
# the five listed in setup.R becomes missing, as in the original code.
years_since_dx <- read_excel(
  file.path(data_dir, years_since_dx_file),
  sheet = study_cohort_sheet
) |>
  check_columns(years_since_dx_columns, years_since_dx_file) |>
  transmute(
    PatientNumber,
    years_since_dx = Years_between_DX_and_Index_Visit,
    years_since_dx_group = factor(Category, levels = years_since_dx_levels)
  )

## ---- read-study-cohort
# One row per patient in the RUSH-era cohort (analysis-plan.md section 4):
# demographics, the year of the index visit, and each outcome before and
# after it
study_raw <- read_excel(
  file.path(data_dir, rush_data_file),
  sheet = study_cohort_sheet
) |>
  check_columns(cohort_columns, rush_data_file)

## ---- merge-years-since-diagnosis
# The original code merged one to one on PatientNumber and kept every row,
# matched or not ("merge 1:1", then "tab _merge"). full_join() keeps every
# row in the same way; relationship = "one-to-one" stops if a patient
# appears twice in either file. The count of matched and unmatched patients
# is printed for the authorized analyst, as "tab _merge" showed it.
study_merged <- study_raw |>
  mutate(in_cohort_sheet = TRUE) |>
  full_join(
    years_since_dx |> mutate(in_years_sheet = TRUE),
    by = "PatientNumber",
    relationship = "one-to-one"
  )
study_merged |>
  count(
    in_cohort_sheet = coalesce(in_cohort_sheet, FALSE),
    in_years_sheet = coalesce(in_years_sheet, FALSE)
  ) |>
  print()

## ---- clean-study-cohort
study_wide <- study_merged |>
  select(-in_cohort_sheet, -in_years_sheet) |>
  clean_cohort(place_columns)

## ---- describe-study-cohort
# The tabulations the original code ran "to confirm that the results are
# the same as in the manuscript". They print counts for the authorized
# analyst; nothing is saved.
for (variable in c("race", "gender", "age_group", "index_test_year",
                   "years_since_dx_group")) {
  print(count(study_wide, .data[[variable]]))
}
print(summary(study_wide$years_since_dx))
print(quantile(
  study_wide$years_since_dx,
  probs = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
  na.rm = TRUE
))

## ---- read-control-cohort
# One row per patient with a qualifying pre-RUSH visit (analysis-plan.md
# section 6). These patients are a subset of the RUSH-era cohort, with the
# same PatientNumber.
control_wide <- read_excel(
  file.path(data_dir, rush_data_file),
  sheet = control_cohort_sheet
) |>
  check_columns(control_columns, str_glue("{rush_data_file}, {control_cohort_sheet}")) |>
  clean_cohort(place_columns)

for (variable in c("race", "gender", "age_group")) {
  print(count(control_wide, .data[[variable]]))
}
