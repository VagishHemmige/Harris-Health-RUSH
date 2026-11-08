# Purpose: compare each outcome before and after the index visit in the
# RUSH-era cohort, overall with McNemar's test and within each
# characteristic as counts and row percentages.
# Needs: study_wide and study_long (R/01_import_data.R,
# R/02_build_long_data.R); outcomes and subgroups_rush (R/setup.R);
# paired_mcnemar() and crosstab_row_percent() (R/functions.R).
# Produces: mcnemar_results (one entry per care outcome) and
# crosstabs[[outcome]][[characteristic]][[period]].

## ---- mcnemar-tests
# McNemar's test on the paired before and after values (analysis-plan.md
# 8.2). The original code ran it for engagement and retention only, "to
# confirm later that clogit gives the same results"; whether to add it for
# viral suppression is open (analysis-plan.md 8.2).
mcnemar_results <- list(
  engaged_in_care = paired_mcnemar(
    study_wide, "engaged_in_care_0", "engaged_in_care_1"
  ),
  retained_in_care = paired_mcnemar(
    study_wide, "retained_in_care_0", "retained_in_care_1"
  )
)

## ---- crosstabs-by-characteristic
# For each outcome and characteristic, the percentage with the outcome in
# each category, after the visit and before it, keeping missing values: the
# raw numbers behind the subgroup results (analysis-plan.md 8.4). The year
# of the visit and years since diagnosis are included, as in the original
# code's loop.
crosstab_characteristics <- c(
  "index_test_year", "race", "gender", "age_group", "african_american",
  "race_group", "years_since_dx_group"
)
crosstabs <- list()
for (outcome in names(outcomes)) {
  for (characteristic in crosstab_characteristics) {
    crosstabs[[outcome]][[characteristic]] <- list(
      after = study_long |>
        filter(post_visit == 1) |>
        crosstab_row_percent(characteristic, outcome),
      before = study_long |>
        filter(post_visit == 0) |>
        crosstab_row_percent(characteristic, outcome)
    )
  }
}
