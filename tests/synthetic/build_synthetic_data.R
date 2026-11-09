# Purpose: write synthetic workbooks with the columns, sheets and value
# formats the scripts expect, so the whole pipeline can be run without the
# study data. Every value is random; nothing comes from the study.
# The race values are invented labels ("Race A" ...), chosen only so the
# study sheet has seven distinct values and the control sheet five, as the
# original code's race codes assume. They say nothing about the real values.
# Usage: build_synthetic_workbooks(folder), called by run_pipeline.R.

build_synthetic_workbooks <- function(folder, n_patients = 600, n_controls = 250,
                                      seed = 2015) {
  set.seed(seed)
  age_groups <- c("16-24", "25-34", "35-44", "45-54", "55+")
  study_race_values <- str_c("Race ", LETTERS[1:7])
  control_race_values <- str_c("Race ", c("A", "B", "C", "D", "G"))

  simulate_cohort <- function(ids, race_values, with_year) {
    n <- length(ids)
    # A patient-level propensity, so outcomes are correlated within patient,
    # and a modest improvement after the visit
    propensity <- rnorm(n, sd = 1.5)
    draw <- function(shift) rbinom(n, 1, plogis(-0.6 + propensity + shift))
    vl_text <- function(shift) {
      value <- if_else(draw(shift) == 1, "Suppressed", "Not Suppressed")
      if_else(runif(n) < 0.4, NA_character_, value)
    }
    cohort <- tibble(
      PatientNumber = ids,
      Race = sample(race_values, n, replace = TRUE,
                    prob = c(0.05, 0.6, rep(0.35 / (length(race_values) - 2),
                                            length(race_values) - 2))),
      Gender = sample(c("Female", "Male"), n, replace = TRUE),
      AgeGroup = sample(age_groups, n, replace = TRUE),
      IndexTestYear = sample(2009:2012, n, replace = TRUE),
      IndexTest_Place = sample(c("Site 1", "Site 2"), n, replace = TRUE),
      EngageInCare_Before = draw(0),
      EngagedInCare_After = draw(0.7),
      RetainedInCare_Before = draw(-0.4),
      RetainedInCare_After = draw(0.3),
      VL_Before = vl_text(-0.8),
      VL_After = vl_text(-0.2),
      EngageInCare_Before_Place = "Clinic",
      EngageInCare_After_Place = "Clinic",
      RetainedInCare_Before_Place = "Clinic",
      RetainedInCare_After_Place = "Clinic"
    )
    if (!with_year) {
      cohort <- select(cohort, -IndexTestYear)
    }
    cohort
  }

  study_ids <- seq_len(n_patients)
  study <- simulate_cohort(study_ids, study_race_values, with_year = TRUE)
  controls <- simulate_cohort(
    sample(study_ids, n_controls), control_race_values, with_year = FALSE
  )
  years <- tibble(
    PatientNumber = study_ids,
    Years_between_DX_and_Index_Visit = sample(1:28, n_patients, replace = TRUE)
  ) |>
    mutate(Category = case_when(
      Years_between_DX_and_Index_Visit <= 5 ~ "<=5",
      Years_between_DX_and_Index_Visit <= 10 ~ "6-10",
      Years_between_DX_and_Index_Visit <= 15 ~ "11-15",
      Years_between_DX_and_Index_Visit <= 20 ~ "16-20",
      .default = ">20"
    ))

  writexl::write_xlsx(
    list(`Study Cohort` = study, Controls = controls),
    file.path(folder, "Deidentified RUSH data 1-15-2015.xlsx")
  )
  writexl::write_xlsx(
    list(`Study Cohort` = years),
    file.path(folder, "Deidentified Years_After_DX 1-15-2015.xlsx")
  )
  invisible(folder)
}
