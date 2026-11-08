# Purpose: helper functions used by the analysis scripts.
# Needs: the packages attached by R/setup.R. Each function takes its inputs
# as arguments and reads no global objects.
# Produces: the functions below.

## ---- check-columns
# Stop if a data frame lacks any expected column. The message names the
# missing columns only, never values.
check_columns <- function(data, expected, source_name) {
  missing_columns <- setdiff(expected, names(data))
  if (length(missing_columns) > 0) {
    stop(str_glue(
      "{source_name} lacks expected columns: ",
      "{str_c(missing_columns, collapse = ', ')}"
    ))
  }
  invisible(data)
}

## ---- check-unique-patients
# Stop if a patient number appears more than once in a one-row-per-patient
# input. Stata's "merge 1:1" and "reshape ..., i(PatientNumber)" stopped on
# duplicates; full_join() and pivot_longer() do not check every row, and a
# duplicated row would change a patient's stratum and the four-row era
# rule. The message gives the number of duplicated patients, never which.
check_unique_patients <- function(data, id_column, source_name) {
  n_duplicated <- data |>
    count(.data[[id_column]]) |>
    filter(n > 1) |>
    nrow()
  if (n_duplicated > 0) {
    stop(str_glue(
      "{source_name}: {n_duplicated} patient number(s) appear more than once."
    ))
  }
  invisible(data)
}

## ---- encode-like-stata
# Stata's encode turns a text variable into numbered categories, numbering
# the distinct values in alphabetical (byte) order, with empty text as
# missing. This returns a factor with its levels in that order, so the
# reference level and the codes match the original code. The codes are
# as.integer() of the factor.
encode_like_stata <- function(x) {
  x <- na_if(x, "")
  value_order <- sort(unique(x[!is.na(x)]), method = "radix")
  factor(x, levels = value_order)
}

## ---- clean-cohort
# The cleaning both cohorts share: drop the unused place columns, number the
# text categories as Stata's encode did, keep each race value's code for
# the race groups (R/02_build_long_data.R), and give the outcome columns
# names ending in _0 (before the visit) and _1 (after it), as the original
# code's renames did before reshaping.
clean_cohort <- function(cohort_raw, place_columns) {
  cohort_raw |>
    select(-all_of(place_columns)) |>
    rename(
      patient_number = PatientNumber,
      engaged_in_care_0 = EngageInCare_Before,
      engaged_in_care_1 = EngagedInCare_After,
      retained_in_care_0 = RetainedInCare_Before,
      retained_in_care_1 = RetainedInCare_After,
      vl_0 = VL_Before,
      vl_1 = VL_After
    ) |>
    # The original code never reads the year in the control sheet, so it
    # is renamed only where present
    rename(any_of(c(index_test_year = "IndexTestYear"))) |>
    mutate(
      race = encode_like_stata(Race),
      race_code = as.integer(race),
      gender = encode_like_stata(Gender),
      age_group = encode_like_stata(AgeGroup),
      # Stata's i.IndexTestYear treats each year as a category, with the
      # earliest as the reference
      across(any_of("index_test_year"), factor)
    ) |>
    select(-Race, -Gender, -AgeGroup)
}

## ---- group-race-codes
# Group race codes into the four race and ethnicity groups, from a named
# list of codes per group (R/setup.R). Codes in no group are missing.
group_race_codes <- function(race_code, codes_by_group) {
  group <- rep(NA_character_, length(race_code))
  for (group_name in names(codes_by_group)) {
    group[race_code %in% codes_by_group[[group_name]]] <- group_name
  }
  factor(group, levels = names(codes_by_group))
}

## ---- reshape-to-long
# One row per patient and period: post_visit is 0 for the period before the
# index visit and 1 for the period after it, as Stata's
# "reshape long ..., j(Intervention)" did with the 0 and 1 suffixes.
reshape_to_long <- function(cohort_wide) {
  cohort_wide |>
    pivot_longer(
      cols = c(
        engaged_in_care_0, engaged_in_care_1,
        retained_in_care_0, retained_in_care_1,
        vl_0, vl_1
      ),
      names_to = c(".value", "post_visit"),
      names_pattern = "(.*)_([01])"
    ) |>
    mutate(post_visit = as.integer(post_visit))
}

## ---- add-viral-load-outcomes
# Two binary viral load outcomes (analysis-plan.md 7.3): vl_suppressed keeps
# a missing result missing; vl_suppressed_missing_fail counts it as not
# suppressed. A value other than the two recorded results is also missing,
# as in the original code.
add_viral_load_outcomes <- function(cohort_long, suppressed, not_suppressed) {
  cohort_long |>
    mutate(
      vl_suppressed = case_when(
        vl == suppressed ~ 1L,
        vl == not_suppressed ~ 0L,
        .default = NA_integer_
      ),
      vl_suppressed_missing_fail = coalesce(vl_suppressed, 0L)
    )
}

## ---- paired-mcnemar
# McNemar's test for one binary outcome measured before and after the visit
# on the same patients, as Stata's mcc reports it: the four paired counts,
# the uncorrected chi-squared test, and the exact test (a binomial test on
# the discordant pairs). Patients missing either value are left out. With
# no complete pairs, or no discordant pairs, there is no test: the
# statistics are missing and n_pairs and n_discordant say why, so the
# pipeline carries on to the other outcomes.
paired_mcnemar <- function(cohort_wide, before, after) {
  pairs <- cohort_wide |>
    filter(!is.na(.data[[before]]), !is.na(.data[[after]]))
  counts <- pairs |>
    count(before = .data[[before]], after = .data[[after]])
  n_after_only <- sum(pairs[[before]] == 0 & pairs[[after]] == 1)
  n_before_only <- sum(pairs[[before]] == 1 & pairs[[after]] == 0)
  n_discordant <- n_after_only + n_before_only
  chi_squared <- NA_real_
  p_value <- NA_real_
  p_value_exact <- NA_real_
  if (n_discordant > 0) {
    chi_squared <- (n_after_only - n_before_only)^2 / n_discordant
    p_value <- pchisq(chi_squared, df = 1, lower.tail = FALSE)
    p_value_exact <- binom.test(n_after_only, n_discordant)$p.value
  }
  list(
    counts = counts,
    n_pairs = nrow(pairs),
    n_discordant = n_discordant,
    proportion_before = if (nrow(pairs) > 0) mean(pairs[[before]]) else NA_real_,
    proportion_after = if (nrow(pairs) > 0) mean(pairs[[after]]) else NA_real_,
    chi_squared = chi_squared,
    p_value = p_value,
    p_value_exact = p_value_exact
  )
}

## ---- crosstab-row-percent
# Counts and row percentages of an outcome within each level of a
# characteristic, keeping missing values, as Stata's
# "tab predictor outcome, row missing" shows them
crosstab_row_percent <- function(cohort_long, predictor, outcome) {
  cohort_long |>
    count(.data[[predictor]], .data[[outcome]]) |>
    group_by(.data[[predictor]]) |>
    mutate(row_percent = 100 * n / sum(n)) |>
    ungroup()
}

## ---- fit-conditional-logit
# Conditional logistic regression with one stratum per patient, the R
# equivalent of Stata's "clogit outcome ..., group(PatientNumber)". The
# right-hand side lists only terms that vary within a patient: a
# characteristic that is the same before and after the visit cannot have a
# main effect within a patient, so Stata drops it; the RUSH-era models do
# not write it (a term written anyway comes back as NA). method = "exact"
# uses the exact conditional likelihood, as Stata does.
# The estimation sample is built as Stata's clogit builds it: rows with
# every variable the model uses, then only patients whose outcome differs
# between their rows, because a patient whose outcome is the same in every
# row (or who has one row) adds nothing to the conditional likelihood and
# Stata drops them ("all positive or all negative outcomes"). Categories
# left with no such patients are then dropped, so the reference category is
# the first one that carries information; the categories present before
# that step are kept in the model as levels_with_data, so
# effect_by_level() can report the dropped ones as not estimable.
fit_conditional_logit <- function(data, outcome, terms) {
  model_formula <- as.formula(
    str_glue("{outcome} ~ {terms} + strata(patient_number)")
  )
  complete_rows <- data |>
    drop_na(all_of(all.vars(model_formula))) |>
    droplevels()
  informative_rows <- complete_rows |>
    group_by(patient_number) |>
    filter(n_distinct(.data[[outcome]]) > 1) |>
    ungroup() |>
    droplevels()
  model <- clogit(model_formula, data = informative_rows, method = "exact")
  model$levels_with_data <- list()
  for (variable in all.vars(model_formula)) {
    if (is.factor(complete_rows[[variable]])) {
      model$levels_with_data[[variable]] <- levels(complete_rows[[variable]])
    }
  }
  model
}

## ---- model-coefficients
# The estimated coefficients of a clogit() model, or the fixed effects of a
# glmer() model, and their covariance matrix
model_coefficients <- function(model) {
  if (inherits(model, "merMod")) {
    estimates <- fixef(model)
  } else {
    estimates <- coef(model)
  }
  list(estimates = estimates, covariance = as.matrix(vcov(model)))
}

## ---- combine-coefficients
# Odds ratio, confidence interval and Wald P value for a sum of
# coefficients, as Stata's "lincom ..., or" reports it. Base R matrix algebra
# is needed for the variance of a sum: it is the sum of the variances and
# covariances of the coefficients in the sum.
combine_coefficients <- function(model, terms, level = 0.95) {
  estimates <- model_coefficients(model)$estimates
  if (!all(terms %in% names(estimates)) || anyNA(estimates[terms])) {
    return(tibble(
      log_odds_ratio = NA_real_, std_error = NA_real_,
      odds_ratio = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      p_value = NA_real_
    ))
  }
  covariance <- model_coefficients(model)$covariance[terms, terms, drop = FALSE]
  log_odds_ratio <- sum(estimates[terms])
  std_error <- sqrt(sum(covariance))
  z <- qnorm(1 - (1 - level) / 2)
  tibble(
    log_odds_ratio = log_odds_ratio,
    std_error = std_error,
    odds_ratio = exp(log_odds_ratio),
    conf_low = exp(log_odds_ratio - z * std_error),
    conf_high = exp(log_odds_ratio + z * std_error),
    p_value = 2 * pnorm(-abs(log_odds_ratio / std_error))
  )
}

## ---- interaction-terms
# The names R gives the coefficients of an interaction between a term and
# each non-reference level of a factor: for example "post_visit:genderMale".
# Built from the levels, so they are found by exact name.
interaction_terms <- function(term, modifier, modifier_levels) {
  str_c(term, ":", modifier, modifier_levels[-1])
}

## ---- effect-by-level
# The odds ratio for the visit (post_visit) within each level of a
# characteristic, from a model with post_visit and its interaction with that
# characteristic. For the reference level it is the post_visit coefficient;
# for each other level it adds that level's interaction coefficient, as the
# original code's lincom commands did. A category that has patients but
# none whose outcome changed was dropped before fitting
# (fit_conditional_logit()); it is listed with estimable = FALSE and missing
# values, rather than left out silently.
effect_by_level <- function(model, modifier, level = 0.95) {
  modifier_levels <- model$xlevels[[modifier]]
  results <- list()
  for (i in seq_along(modifier_levels)) {
    terms <- "post_visit"
    if (i > 1) {
      terms <- c(terms, str_c("post_visit:", modifier, modifier_levels[i]))
    }
    results[[modifier_levels[i]]] <- combine_coefficients(model, terms, level)
  }
  levels_with_data <- model$levels_with_data[[modifier]]
  if (is.null(levels_with_data)) {
    levels_with_data <- modifier_levels
  }
  bind_rows(results, .id = "level") |>
    mutate(estimable = !is.na(odds_ratio)) |>
    right_join(tibble(level = levels_with_data), by = "level") |>
    mutate(
      estimable = coalesce(estimable, FALSE),
      level = factor(level, levels = levels_with_data)
    ) |>
    arrange(level) |>
    mutate(level = as.character(level))
}

## ---- wald-test
# Joint Wald test that a set of coefficients are all zero, as Stata's
# testparm reports it. Coefficients R could not estimate (NA, as when a
# level has no information) are left out, as Stata omits them; the degrees
# of freedom are the number tested.
wald_test <- function(model, terms) {
  estimates <- model_coefficients(model)$estimates
  terms <- terms[terms %in% names(estimates) & !is.na(estimates[terms])]
  if (length(terms) == 0) {
    return(tibble(chi_squared = NA_real_, df = 0L, p_value = NA_real_))
  }
  # Base R matrix algebra: the Wald statistic b' V^-1 b
  b <- estimates[terms]
  covariance <- model_coefficients(model)$covariance[terms, terms, drop = FALSE]
  chi_squared <- as.numeric(t(b) %*% solve(covariance) %*% b)
  tibble(
    chi_squared = chi_squared,
    df = length(terms),
    p_value = pchisq(chi_squared, df = length(terms), lower.tail = FALSE)
  )
}

## ---- tidy-odds-ratios
# Every coefficient as an odds ratio with a Wald confidence interval and P
# value, as Stata's "or" option shows them. Works for clogit() and glmer()
# models; for glmer() these are the fixed effects.
tidy_odds_ratios <- function(model, level = 0.95) {
  estimates <- model_coefficients(model)$estimates
  results <- list()
  for (term in names(estimates)) {
    results[[term]] <- combine_coefficients(model, term, level)
  }
  bind_rows(results, .id = "term")
}
