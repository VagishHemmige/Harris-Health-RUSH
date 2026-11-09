# Purpose: reshape each cohort to one row per patient and period, add the
# viral load outcomes and the race groups, and stack the two eras.
# Needs: study_wide and control_wide (R/01_import_data.R); the viral load
# values, race code lists and african_american_code (R/setup.R);
# reshape_to_long(), add_viral_load_outcomes() and group_race_codes()
# (R/functions.R).
# Produces: study_long (RUSH-era cohort, two rows per patient), control_long
# (pre-RUSH visits, two rows per patient) and both_eras_long (the two
# stacked, up to four rows per patient).

## ---- reshape-study-cohort
# Two rows per patient: post_visit = 0 for the period before the index
# visit, 1 for the period after it (analysis-plan.md section 7)
study_long <- study_wide |>
  reshape_to_long() |>
  add_viral_load_outcomes(vl_suppressed_value, vl_not_suppressed_value)

## ---- group-race-study
# The original code's AfricanAmerican indicator and four race groups, from
# encode()'s race codes in the study sheet (R/setup.R). Stata's
# "generate AfricanAmerican=(Race==2)" is 0, not missing, when race is
# missing, so coalesce() makes a missing race "No" here too; the race
# groups stay missing. The table that
# follows shows which recorded value each code stands for, so an
# authorized analyst can check the grouping, as the original code's
# "tab NewRace Race" did.
study_long <- study_long |>
  mutate(
    african_american = factor(
      if_else(coalesce(race_code == african_american_code, FALSE), "Yes", "No"),
      levels = c("No", "Yes")
    ),
    race_group = group_race_codes(race_code, race_group_codes_study),
    rush_era = 1L
  )
study_long |>
  distinct(patient_number, race_code, race, race_group) |>
  count(race_code, race, race_group) |>
  print()

## ---- reshape-control-cohort
control_long <- control_wide |>
  reshape_to_long() |>
  add_viral_load_outcomes(vl_suppressed_value, vl_not_suppressed_value)

## ---- group-race-control
# The same indicator and groups from the control sheet's own race codes,
# which encode() numbered from the values present in that sheet
control_long <- control_long |>
  mutate(
    african_american = factor(
      if_else(coalesce(race_code == african_american_code, FALSE), "Yes", "No"),
      levels = c("No", "Yes")
    ),
    race_group = group_race_codes(race_code, race_group_codes_control),
    rush_era = 0L
  )
control_long |>
  distinct(patient_number, race_code, race, race_group) |>
  count(race_code, race, race_group) |>
  print()

## ---- stack-eras
# Both eras together, sorted by patient, era and period, as the original
# code's append and sort. The two sheets' race codes are not comparable, so
# the stacked data use the race groups, never the codes. Sex and age group
# are stacked as their recorded text (factor levels), so a level missing
# from one sheet cannot shift the other's categories.
both_eras_long <- bind_rows(
  study_long |> mutate(race = as.character(race)),
  control_long |> mutate(race = as.character(race))
) |>
  mutate(
    gender = fct_relevel(factor(gender), levels(study_long$gender)),
    age_group = fct_relevel(factor(age_group), levels(study_long$age_group)),
    race_group = factor(race_group, levels = race_group_levels)
  ) |>
  arrange(patient_number, rush_era, post_visit)
