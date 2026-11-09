# Purpose: fit the conditional logistic regressions for the RUSH-era cohort:
# the effect of the index visit on each outcome, whether it differs by each
# characteristic, and the multivariable models.
# Needs: study_long (R/02_build_long_data.R); outcomes, subgroups_rush,
# skipped_rush_subgroup_models and confidence_level (R/setup.R);
# fit_conditional_logit(), tidy_odds_ratios(), wald_test(),
# interaction_terms() and effect_by_level() (R/functions.R).
# Produces: visit_models and visit_effects; subgroup_models,
# subgroup_tests and subgroup_effects, each indexed
# [[characteristic]][[outcome]]; combined_models, combined_tests and
# combined_effects; multivariable_models and multivariable_effects.

## ---- fit-visit-models
# The effect of the visit on each outcome (analysis-plan.md 8.3): one
# stratum per patient, so each patient is compared with themself before and
# after. The odds ratio for post_visit is the conditional odds ratio; for a
# before-and-after pair it rests only on the patients whose outcome changed,
# like McNemar's test.
visit_models <- list()
for (outcome in names(outcomes)) {
  visit_models[[outcome]] <- fit_conditional_logit(
    study_long, outcome, "post_visit"
  )
}
visit_effects <- list()
for (outcome in names(outcomes)) {
  visit_effects[[outcome]] <- tidy_odds_ratios(
    visit_models[[outcome]], confidence_level
  )
}
visit_effects <- bind_rows(visit_effects, .id = "outcome")

## ---- fit-subgroup-models
# For each characteristic and outcome (analysis-plan.md 8.4), a model with
# the visit and its interaction with the characteristic, Stata's
# "i.Intervention##i.<characteristic>". The characteristic's own main
# effect is not written: it does not change within a patient, so it cannot
# be estimated. Models the original code could not fit are skipped
# (R/setup.R).
subgroup_models <- list()
for (subgroup in names(subgroups_rush)) {
  for (outcome in names(outcomes)) {
    skipped <- skipped_rush_subgroup_models |>
      filter(.data$outcome == .env$outcome, .data$subgroup == .env$subgroup)
    if (nrow(skipped) > 0) {
      next
    }
    subgroup_models[[subgroup]][[outcome]] <- fit_conditional_logit(
      study_long,
      outcome,
      str_glue("post_visit + post_visit:{subgroup}")
    )
  }
}

## ---- test-subgroup-interactions
# Does the visit's effect differ across the categories? A joint Wald test
# that every interaction coefficient is zero, as the original code's
# testparm. For a two-category characteristic it is the test of the single
# interaction coefficient.
subgroup_tests <- list()
for (subgroup in names(subgroup_models)) {
  for (outcome in names(subgroup_models[[subgroup]])) {
    model <- subgroup_models[[subgroup]][[outcome]]
    subgroup_tests[[subgroup]][[outcome]] <- wald_test(
      model,
      interaction_terms("post_visit", subgroup, model$xlevels[[subgroup]])
    )
  }
}

## ---- estimate-subgroup-effects
# The odds ratio for the visit within each category: the post_visit
# coefficient for the reference category, plus that category's interaction
# coefficient for the others, as the original code's lincom commands
subgroup_effects <- list()
for (subgroup in names(subgroup_models)) {
  for (outcome in names(subgroup_models[[subgroup]])) {
    subgroup_effects[[subgroup]][[outcome]] <- effect_by_level(
      subgroup_models[[subgroup]][[outcome]], subgroup, confidence_level
    )
  }
}

## ---- fit-combined-models
# Years since diagnosis and age group in one model, each interacting with
# the visit, to see whether the differences by years since diagnosis remain
# once age is allowed for (analysis-plan.md 8.4)
combined_models <- list()
combined_tests <- list()
combined_effects <- list()
for (outcome in names(outcomes)) {
  model <- fit_conditional_logit(
    study_long,
    outcome,
    "post_visit + post_visit:years_since_dx_group + post_visit:age_group"
  )
  combined_models[[outcome]] <- model
  for (subgroup in c("years_since_dx_group", "age_group")) {
    combined_tests[[outcome]][[subgroup]] <- wald_test(
      model,
      interaction_terms("post_visit", subgroup, model$xlevels[[subgroup]])
    )
    combined_effects[[outcome]][[subgroup]] <- effect_by_level(
      model, subgroup, confidence_level
    )
  }
}

## ---- fit-multivariable-models
# All characteristics together, each interacting with the visit
# (analysis-plan.md 8.5): once with the African American indicator and once
# with race as recorded. The original code fitted these for three outcomes,
# leaving out viral suppression with missing results excluded.
multivariable_outcomes <- c(
  "vl_suppressed_missing_fail", "engaged_in_care", "retained_in_care"
)
multivariable_terms <- c(
  african_american = str_c(
    "post_visit + post_visit:index_test_year + post_visit:gender + ",
    "post_visit:age_group + post_visit:african_american"
  ),
  race = str_c(
    "post_visit + post_visit:index_test_year + post_visit:gender + ",
    "post_visit:age_group + post_visit:race"
  )
)
multivariable_models <- list()
multivariable_effects <- list()
for (version in names(multivariable_terms)) {
  for (outcome in multivariable_outcomes) {
    model <- fit_conditional_logit(
      study_long, outcome, multivariable_terms[[version]]
    )
    multivariable_models[[version]][[outcome]] <- model
    multivariable_effects[[version]][[outcome]] <- tidy_odds_ratios(
      model, confidence_level
    )
  }
}
