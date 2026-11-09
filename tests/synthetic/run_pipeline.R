# Purpose: run R/main.R on synthetic workbooks and check that the helpers
# give the answers known from theory. Uses no study data.
# Run from the project root: Rscript tests/synthetic/run_pipeline.R
# Needs, beyond the packages in R/setup.R: writexl.

synthetic_dir <- file.path(tempdir(), "rush-synthetic")
# The tables go to tables/ in the project, which is gitignored
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

# 4b. The era subgroup models equal Stata's full factorial
# i.Intervention##i.RUSHEra##i.Age. In the synthetic data age group is drawn
# separately for each era, so it changes within many patients and its main
# effect must be estimated, not dropped.
full_factorial <- clogit(
  engaged_in_care ~ post_visit * rush_era * age_group + strata(patient_number),
  data = era_samples$both_eras,
  method = "exact"
)
from_script <- era_subgroup_models$both_eras$age_group$engaged_in_care
checks$era_subgroup_model_is_full_factorial <- isTRUE(all.equal(
  coef(from_script), coef(full_factorial), tolerance = 1e-6
)) && !anyNA(coef(from_script)[str_c("age_group", levels(era_samples$both_eras$age_group)[-1])])

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

# 7. A category with no informative patients (only concordant pairs, or
# only one row each) is dropped before fitting and reported as not
# estimable; the reference becomes the first informative category. Invented
# pairs: A (first level) one (0,0) pair; B three (0,1) and one (1,0), odds
# ratio 3; C two of each, odds ratio 1; D single rows only.
make_pairs <- function(group, before, after, first_id) {
  ids <- first_id + seq_along(before) - 1
  tibble(
    patient_number = rep(ids, each = 2),
    group = group,
    post_visit = rep(c(0L, 1L), times = length(before)),
    y = as.vector(rbind(before, after))
  )
}
uninformative <- bind_rows(
  make_pairs("A", 0, 0, 1),
  make_pairs("B", c(0, 0, 0, 1), c(1, 1, 1, 0), 10),
  make_pairs("C", c(0, 0, 1, 1), c(1, 1, 0, 0), 20),
  tibble(patient_number = 30:31, group = "D", post_visit = c(0L, 1L), y = c(0, 1))
) |>
  mutate(group = factor(group, levels = c("A", "B", "C", "D")))
uninformative_effects <- fit_conditional_logit(
  uninformative, "y", "post_visit + post_visit:group"
) |>
  effect_by_level("group")
checks$uninformative_reference_category <- isTRUE(all.equal(
  uninformative_effects$level, c("A", "B", "C", "D")
)) &&
  isTRUE(all.equal(uninformative_effects$estimable, c(FALSE, TRUE, TRUE, FALSE))) &&
  isTRUE(all.equal(uninformative_effects$odds_ratio[2:3], c(3, 1), tolerance = 1e-6))

# 8. A repeated patient number stops the import: in the control sheet, and
# in the study sheet for a patient absent from the years workbook (which
# the join's relationship check alone would not catch).
stops <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
duplicate_controls <- control_wide |>
  rename(PatientNumber = patient_number) |>
  slice(c(1, 1, 2))
unmatched_duplicate <- tibble(PatientNumber = c(99999, 99999, 1))
checks$duplicate_patients_stop <- stops(
  check_unique_patients(duplicate_controls, "PatientNumber", "controls")
) && stops(
  check_unique_patients(unmatched_duplicate, "PatientNumber", "study")
) && !stops(
  check_unique_patients(tibble(PatientNumber = 1:3), "PatientNumber", "ok")
)

# 9. McNemar's test with no discordant pairs, or no complete pairs, returns
# missing statistics instead of stopping the pipeline.
concordant <- tibble(before = c(0, 1, 1), after = c(0, 1, 1))
no_pairs <- tibble(before = c(NA, 1), after = c(0, NA))
concordant_result <- paired_mcnemar(concordant, "before", "after")
no_pairs_result <- paired_mcnemar(no_pairs, "before", "after")
checks$mcnemar_without_discordant_pairs <-
  concordant_result$n_discordant == 0 && is.na(concordant_result$p_value) &&
  is.na(concordant_result$p_value_exact) &&
  no_pairs_result$n_pairs == 0 && is.na(no_pairs_result$chi_squared)

# 10. Tables 2 to 4 take their interval and P value from the model, which
# for paired data are the Wald ones known from theory: the log of the
# ratio of discordant pairs, plus or minus 1.96 times the square root of
# 1/b + 1/c. The Total row shows exactly that interval.
n_better <- sum(pairs$engaged_in_care_0 == 0 & pairs$engaged_in_care_1 == 1)
n_worse <- sum(pairs$engaged_in_care_0 == 1 & pairs$engaged_in_care_1 == 0)
wald_se <- sqrt(1 / n_better + 1 / n_worse)
total_effect <- visit_effects |>
  filter(outcome == "engaged_in_care", term == "post_visit")
checks$visit_interval_is_wald <- isTRUE(all.equal(
  c(total_effect$conf_low, total_effect$conf_high),
  exp(log(n_better / n_worse) + c(-1, 1) * qnorm(0.975) * wald_se),
  tolerance = 1e-6
))
total_row <- outcome_table_rows$engaged_in_care |>
  filter(row_label == "Total")
checks$table_total_uses_model_interval <- total_row$odds_ratio == str_glue(
  "{style_ratio(total_effect$odds_ratio)} ",
  "({style_ratio(total_effect$conf_low)}–{style_ratio(total_effect$conf_high)})"
)

# 11. A category in which no patient got worse prints an infinite odds
# ratio with no P value, as the article's Table 4 does, whatever very large
# number the model returns for it.
separated_counts <- count_pairs(tibble(before = c(0, 0, 1), after = c(1, 1, 1)))
huge_effect <- tibble(odds_ratio = 1e9, conf_low = 0, conf_high = Inf, p_value = 0.998)
separated_row <- format_change(separated_counts, huge_effect)
checks$separated_category_prints_infinity <-
  separated_row$odds_ratio == "∞" && separated_row$p_value == "-"

# 12. The four tables were built and saved.
checks$tables_saved <- all(file.exists(file.path(
  tables_dir, str_glue("table_{1:4}.html")
))) && all(map_lgl(published_style_tables, \(x) inherits(x, "gtsummary")))

results <- tibble(check = names(checks), passed = unlist(checks))
print(results, n = Inf)
if (!all(results$passed)) {
  stop("Some synthetic checks failed.")
}
cat("All synthetic checks passed.\n")
