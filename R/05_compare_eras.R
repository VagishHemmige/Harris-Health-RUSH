# Purpose: compare the change around an emergency department visit in the
# RUSH era with the change around a visit in the pre-RUSH era, first with
# every patient and then with only the patients who had a visit in both
# eras.
# Needs: both_eras_long (R/02_build_long_data.R); outcomes, subgroups_era,
# skipped_era_subgroup_models_all, quadrature_points,
# rows_per_patient_both_eras and confidence_level (R/setup.R);
# fit_conditional_logit(), tidy_odds_ratios(), interaction_terms() and
# wald_test() (R/functions.R).
# Produces, for each of the two samples ("all_patients" and "both_eras"):
# era_models, era_effects, era_subgroup_models, era_subgroup_tests,
# random_effects_models and random_effects_effects, each indexed
# [[sample]][[outcome]] (subgroups [[sample]][[characteristic]][[outcome]]).

## ---- define-era-samples
# The original code fitted every era model twice (analysis-plan.md 8.6):
# first "by request" with every patient, including those with a RUSH-era
# visit only, and then with the patients who had four rows, a visit in each
# era. A RUSH-era-only patient's two rows inform the RUSH-era change but not
# the comparison with the earlier era.
era_samples <- list(
  all_patients = both_eras_long,
  both_eras = both_eras_long |>
    add_count(patient_number, name = "rows_per_patient") |>
    filter(rows_per_patient == rows_per_patient_both_eras) |>
    select(-rows_per_patient)
)
# Patients by rows each, as the original code's "tab NumObs"
both_eras_long |>
  count(patient_number, name = "rows_per_patient") |>
  count(rows_per_patient) |>
  print()

## ---- fit-era-models
# Within each patient: post_visit is the change around a pre-RUSH visit,
# rush_era the difference between the periods before each era's visit,
# and post_visit:rush_era how much larger (as a ratio of odds ratios) the
# change around the RUSH-era visit was. The interaction is the test the
# article reports for each outcome.
era_models <- list()
era_effects <- list()
for (sample in names(era_samples)) {
  for (outcome in names(outcomes)) {
    model <- fit_conditional_logit(
      era_samples[[sample]], outcome, "post_visit * rush_era"
    )
    era_models[[sample]][[outcome]] <- model
    era_effects[[sample]][[outcome]] <- tidy_odds_ratios(model, confidence_level)
  }
}

## ---- fit-era-subgroup-models
# Whether any of the era terms differ by a characteristic: every term of
# the era model interacted with it, Stata's
# "i.Intervention##i.RUSHEra##i.<characteristic>", and one joint Wald test
# of all the interaction terms, as the original code's testparm. The
# characteristic's own main effect is kept: each era's rows take the
# characteristic from that era's sheet, so age group, for example, can
# differ between a patient's pre-RUSH and RUSH-era rows, and then the main
# effect is estimable. Where it is constant within every patient, clogit()
# returns it as NA, as Stata omits it. Models the original code could not
# fit with every patient are skipped there (R/setup.R).
era_subgroup_models <- list()
era_subgroup_tests <- list()
for (sample in names(era_samples)) {
  for (subgroup in names(subgroups_era)) {
    for (outcome in names(outcomes)) {
      skipped <- skipped_era_subgroup_models_all |>
        filter(.data$outcome == .env$outcome, .data$subgroup == .env$subgroup)
      if (sample == "all_patients" && nrow(skipped) > 0) {
        next
      }
      model <- fit_conditional_logit(
        era_samples[[sample]],
        outcome,
        str_glue("post_visit * rush_era * {subgroup}")
      )
      era_subgroup_models[[sample]][[subgroup]][[outcome]] <- model
      subgroup_levels <- model$xlevels[[subgroup]]
      era_subgroup_tests[[sample]][[subgroup]][[outcome]] <- wald_test(
        model,
        c(
          interaction_terms("post_visit", subgroup, subgroup_levels),
          interaction_terms("rush_era", subgroup, subgroup_levels),
          interaction_terms("post_visit:rush_era", subgroup, subgroup_levels)
        )
      )
    }
  }
}

## ---- fit-random-effects-models
# For comparison, the same era model with a random intercept for each
# patient instead of a stratum, the R equivalent of Stata's xtlogit. Unlike
# the conditional model, it also uses the differences between patients, so
# it assumes the random intercept is unrelated to the visit and era.
random_effects_models <- list()
random_effects_effects <- list()
for (sample in names(era_samples)) {
  for (outcome in names(outcomes)) {
    model <- glmer(
      as.formula(str_glue("{outcome} ~ post_visit * rush_era + (1 | patient_number)")),
      data = era_samples[[sample]],
      family = binomial,
      nAGQ = quadrature_points
    )
    random_effects_models[[sample]][[outcome]] <- model
    random_effects_effects[[sample]][[outcome]] <- tidy_odds_ratios(
      model, confidence_level
    )
  }
}
