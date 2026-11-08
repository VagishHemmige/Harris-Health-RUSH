# Purpose: run R/main.R on synthetic workbooks and check that the helpers
# give the answers known from theory. Uses no study data.
# Run from the project root: Rscript tests/synthetic/run_pipeline.R
# Needs, beyond the packages in R/setup.R: writexl.

synthetic_dir <- file.path(tempdir(), "rush-synthetic")
dir.create(synthetic_dir, showWarnings = FALSE)

source("R/setup.R")
source("tests/synthetic/build_synthetic_data.R")
build_synthetic_workbooks(synthetic_dir)
Sys.setenv(RUSH_DATA_DIR = synthetic_dir)

source("R/main.R")

checks <- list()

# 1. For before-and-after pairs, the conditional odds ratio is the ratio of
# the two kinds of discordant pair, the quantity McNemar's test examines.
pairs <- study_wide |>
  filter(!is.na(engaged_in_care_0), !is.na(engaged_in_care_1))
discordant_ratio <- sum(pairs$engaged_in_care_0 == 0 & pairs$engaged_in_care_1 == 1) /
  sum(pairs$engaged_in_care_0 == 1 & pairs$engaged_in_care_1 == 0)
checks$clogit_matches_discordant_ratio <- isTRUE(all.equal(
  exp(coef(visit_models$engaged_in_care)[["post_visit"]]),
  discordant_ratio,
  tolerance = 1e-6
))

# 2. The odds ratio for the visit within each age group, from the
# interaction model, equals the one from a model fitted to that age group
# alone.
for (age in levels(study_long$age_group)) {
  separate <- fit_conditional_logit(
    filter(study_long, age_group == age), "engaged_in_care", "post_visit"
  )
  from_interaction <- subgroup_effects$age_group$engaged_in_care |>
    filter(level == age)
  checks[[str_glue("effect_by_level_{age}")]] <- isTRUE(all.equal(
    from_interaction$odds_ratio,
    exp(coef(separate)[["post_visit"]]),
    tolerance = 1e-6
  ))
}

# 3. For one coefficient, wald_test() equals the model's own Wald test
# (survival rounds its statistic to two decimals, hence the tolerance), and
# the square of the coefficient's z statistic.
own_wald <- summary(visit_models$retained_in_care)$waldtest
checks$wald_test_matches_survival <- isTRUE(all.equal(
  wald_test(visit_models$retained_in_care, "post_visit")$chi_squared,
  unname(own_wald[["test"]]),
  tolerance = 1e-3
)) && isTRUE(all.equal(
  wald_test(visit_models$retained_in_care, "post_visit")$chi_squared,
  (coef(visit_models$retained_in_care)[["post_visit"]])^2 /
    vcov(visit_models$retained_in_care)[["post_visit", "post_visit"]]
))

# 4. Every interaction test found its coefficients (a misspelled
# coefficient name would leave df at 0).
interaction_dfs <- c(
  map_int(list_flatten(subgroup_tests), "df"),
  map_int(list_flatten(list_flatten(era_subgroup_tests)), "df")
)
checks$interaction_terms_found <- all(interaction_dfs > 0)

# 5. The restricted era sample has four rows for every patient.
checks$both_eras_four_rows <- era_samples$both_eras |>
  count(patient_number) |>
  pull(n) |>
  (\(n) all(n == rows_per_patient_both_eras))()

# 6. McNemar's chi-squared matches R's own uncorrected test.
own_mcnemar <- mcnemar.test(
  table(pairs$engaged_in_care_0, pairs$engaged_in_care_1),
  correct = FALSE
)
checks$mcnemar_matches_stats <- isTRUE(all.equal(
  mcnemar_results$engaged_in_care$chi_squared,
  unname(own_mcnemar$statistic)
))

results <- tibble(check = names(checks), passed = unlist(checks))
print(results, n = Inf)
if (!all(results$passed)) {
  stop("Some synthetic checks failed.")
}
cat("All synthetic checks passed.\n")
