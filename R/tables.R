# Purpose: build the article's Tables 1 to 4 from the data with gtsummary:
# Table 1, the characteristics of the two cohorts; Tables 2 to 4, each
# outcome before and after the RUSH-era visit, by subgroup, with the
# matched-pairs odds ratio and the test of difference across groups.
# Needs: study_long and control_long (R/02_build_long_data.R);
# subgroup_tests (R/04_fit_rush_era_models.R); the table constants
# (R/setup.R); count_categories(), paired_mcnemar(), pair_outcome(),
# format_change(), relabel_levels(), format_table_1() and
# format_outcome_table() (R/functions.R).
# Produces: published_style_tables, a named list of four gtsummary tables,
# and the rows behind them (table_1_rows, outcome_table_rows); writes HTML
# copies to tables/, which is gitignored: tables made from the study data
# are not published.

## ---- define-patient-characteristics
# One row per patient in each cohort, with the characteristics the tables
# use. They are the same in a patient's before and after rows.
study_patients <- study_long |>
  select(patient_number, any_of(names(table_characteristics))) |>
  distinct()
# The controls are the same patients as the study cohort, with the same
# patient numbers, so their years since diagnosis come from the years
# workbook through the study cohort. That workbook holds years from
# diagnosis to the RUSH-era index visit, so this is the value used.
control_patients <- control_long |>
  select(patient_number, any_of(names(table_characteristics))) |>
  distinct() |>
  left_join(
    study_patients |> select(patient_number, years_since_dx_group),
    by = "patient_number",
    relationship = "one-to-one"
  )

## ---- count-table-1
# Counts and column percentages for each category of each characteristic,
# in each cohort (Table 1). The control sheet need not carry the visit
# year; if it does not, those cells stay empty.
# Visit years are paired by position in each era (2009 with 2004, and so
# on), as the article's Table 1 pairs them.
table_1_rows <- list()
for (characteristic in names(table_characteristics)) {
  labels <- table_level_labels[[characteristic]]
  study_counts <- count_categories(study_patients, characteristic, labels)
  control_counts <- count_categories(control_patients, characteristic, labels)
  if (characteristic == "index_test_year") {
    study_counts <- study_counts |> mutate(position = row_number())
    control_counts <- control_counts |>
      mutate(position = row_number()) |>
      rename(control_year = row_label)
    rows <- study_counts |>
      left_join(control_counts, by = "position", suffix = c("_study", "_control")) |>
      mutate(row_label = if_else(
        is.na(control_year),
        row_label,
        str_glue("{row_label} ({control_year} for Control)")
      )) |>
      select(-position, -control_year)
  } else {
    rows <- study_counts |>
      left_join(control_counts, by = "row_label", suffix = c("_study", "_control"))
  }
  table_1_rows[[characteristic]] <- rows |>
    transmute(
      group = table_characteristics[[characteristic]],
      row_label = as.character(row_label),
      study_n = as.character(n_study),
      study_pct = pct_study,
      control_n = as.character(n_control),
      control_pct = pct_control
    )
}
table_1_rows <- bind_rows(
  bind_rows(table_1_rows),
  tibble(
    group = "Total", row_label = "Total",
    study_n = as.character(nrow(study_patients)), study_pct = "",
    control_n = as.character(nrow(control_patients)), control_pct = ""
  )
)

## ---- compute-outcome-table-rows
# For each outcome, characteristic and category: patients with both values,
# the number and percentage with the outcome before and after the visit,
# McNemar's matched-pairs odds ratio with its exact interval, and the
# McNemar chi-squared P value (paired_mcnemar()). The test across groups is
# the joint Wald test of the visit-by-characteristic interaction from
# R/04_fit_rush_era_models.R, shown once per characteristic. Whether these
# are the exact statistics behind the article's columns is part of issue #5.
outcome_table_rows <- list()
for (outcome in names(table_outcomes)) {
  pairs <- pair_outcome(study_long, outcome, names(table_characteristics))
  rows <- list()
  for (characteristic in names(table_characteristics)) {
    test <- subgroup_tests[[characteristic]][[outcome]]
    levels_present <- pairs |>
      filter(!is.na(.data[[characteristic]])) |>
      pull(.data[[characteristic]]) |>
      unique() |>
      sort()
    for (i in seq_along(levels_present)) {
      level_pairs <- pairs |>
        filter(.data[[characteristic]] == levels_present[i])
      rows[[str_c(characteristic, "_", i)]] <- format_change(
        paired_mcnemar(level_pairs, "before", "after")
      ) |>
        mutate(
          group = table_characteristics[[characteristic]],
          row_label = relabel_levels(
            as.character(levels_present[i]),
            table_level_labels[[characteristic]]
          ),
          chi_squared = if (i == 1) style_number(test$chi_squared, digits = 2) else "",
          chi_p = if (i == 1) style_pvalue(test$p_value, digits = 3) else ""
        )
    }
  }
  rows[["total"]] <- format_change(paired_mcnemar(pairs, "before", "after")) |>
    mutate(group = "Total", row_label = "Total", chi_squared = "", chi_p = "")
  outcome_table_rows[[outcome]] <- bind_rows(rows) |>
    relocate(group, row_label)
}

## ---- build-tables
# The four tables, formatted by the same functions that format the
# published values on the website
published_style_tables <- list(
  table_1 = format_table_1(
    table_1_rows,
    study_header = str_glue("**Study period cohort (N = {nrow(study_patients)})**"),
    control_header = str_glue(
      "**Control period cohort subset (N = {nrow(control_patients)})**"
    )
  )
)
for (outcome in names(table_outcomes)) {
  published_style_tables[[table_numbers[[outcome]]]] <- format_outcome_table(
    outcome_table_rows[[outcome]], table_outcomes[[outcome]]
  )
}

## ---- save-tables
# HTML copies for the authorized analyst; tables/ is gitignored
dir.create(tables_dir, showWarnings = FALSE)
for (name in names(published_style_tables)) {
  published_style_tables[[name]] |>
    as_gt() |>
    gt::gtsave(file.path(tables_dir, str_glue("{name}.html")))
}
