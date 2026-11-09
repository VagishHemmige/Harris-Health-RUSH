# Site-only helper: shows parts of the original Stata file on the website.
# It reads stata/RUSH_analysis_2015-01-15.do as text and never runs it or
# reads data. Each part starts at a comment line of the .do file (its
# "anchor", matched exactly, searching on from the previous part's start,
# since some comments repeat) and runs to the line before the next part.
# Pages source this file from quarto/, so paths are relative to it.

stata_file <- "../stata/RUSH_analysis_2015-01-15.do"

stata_parts <- data.frame(
  id = c(
    "setup", "years", "import", "clean", "tabs", "rename", "mcnemar",
    "reshape", "race", "crosstabs", "visit", "year", "ysd", "gender", "age",
    "combined", "race_models", "new_race", "african_american",
    "multivariable", "controls", "stack", "era_all", "era_all_subgroups",
    "era_all_random", "restrict", "era_both", "era_both_subgroups",
    "era_both_random"
  ),
  title = c(
    "Header, setup and log",
    "Years since diagnosis",
    "Import the study cohort and merge",
    "Clean and encode the variables",
    "Tabulations to check against the manuscript",
    "Rename the outcomes for reshaping",
    "McNemar's tests",
    "Reshape to long; viral load outcomes",
    "African American indicator, NewRace and RUSH era",
    "Crosstabs",
    "The effect of the visit",
    "Interaction with year of visit",
    "Interaction with years since diagnosis",
    "Interaction with gender",
    "Interaction with age",
    "Years since diagnosis and age together",
    "Interaction with race as recorded",
    "Interaction with NewRace",
    "Interaction with the African American indicator",
    "Multivariable models",
    "Prepare the control era",
    "Stack the two eras",
    "Era comparison, every patient",
    "Era comparison by characteristic, every patient",
    "Random-effects models, every patient",
    "Keep patients with both eras",
    "Era comparison, both-era patients",
    "Era comparison by characteristic, both-era patients",
    "Random-effects models, both-era patients"
  ),
  anchor = c(
    "//Original Stata analysis for the RUSH study (JAIDS 2015, doi:10.1097/QAI.0000000000000578).",
    "//We first import, clean, and then save as a data file the \"Years since diagnosis\" data",
    "//We first import data from the main file from Excel and merge with the Years since diagnosis file:",
    "//Now we clean variables and convert strings to numerics.  We drop unnecessary variables.",
    "//We do the appropriate tabs to confirm that the results are the same as in the manuscript",
    "//We now rename variables in preparation of converting to long format, as required by clogit",
    "//We perform McNemer's test on EngagedInCare and RetainedInCare to confirm later that clogit gives the same results",
    "//Now we reshape the dataset to long",
    "//We now create an AfricanAmerican variable from Race",
    "//First, we set loops to run the crosstabs for the raw analysis:",
    "//We now run the conditional logistic regressions",
    "//These regressions test whether the intervention's efficacy varies by year and calculates out the interaction terms.",
    "////We now do the same for YearsSinceDiagnosisCategorical, seeing whether intervention efficacy varies by Years since diagnosis.",
    "//We now do the same for Gender, seeing whether intervention efficacy varies by Gender.",
    "//We now will do the same for Age:",
    "////We now do the same for YearsSinceDiagnosisCategorical and age in the same regression, seeing whether intervention efficacy varies by Years since diagnosis.",
    "//We do the same for Race:",
    "//And for NewRace:",
    "//And for AfricanAmerican",
    "//Now we put it all together into a final regression model including all variables, initially with the African American variable:",
    "//Now we compare with the control era",
    "//Now we do the analysis to see if the intervention in the RUSH era is more effective than the intervention in the Pre-RUSH ERA",
    "//We run fixed effects regression by request including all patients in RUSH as well as the pre-RUSH controls:",
    "//Now we include the interactions with each of the variables of interest (this becomes ridiculously complicated, but is included for completeness)",
    "//In comparison, now we run random-effects regressions:",
    "//Now, we count the number of observations for each patient and only keep if they have a Pre-Rush Control",
    "//Now, we actually run the conditional logistic regression, with a variable for intervention and a second variable for RUSH era, as well as an interaction",
    "//Now we include the interactions with each of the variables of interest (this becomes ridiculously complicated, but is included for completeness)",
    "//And for comparison, we again run random-effects regressions:"
  ),
  page = c(
    "setup.qmd", "01_import_data.qmd", "01_import_data.qmd",
    "01_import_data.qmd", "01_import_data.qmd", "01_import_data.qmd",
    "03_paired_comparisons.qmd", "02_build_long_data.qmd",
    "02_build_long_data.qmd", "03_paired_comparisons.qmd",
    rep("04_fit_rush_era_models.qmd", 10),
    "01_import_data.qmd", "02_build_long_data.qmd",
    rep("05_compare_eras.qmd", 7)
  ),
  stringsAsFactors = FALSE
)

# Find each part's first and last line. Stops if an anchor is missing, so a
# change to the .do file cannot silently shift the parts.
locate_stata_parts <- function(lines, parts) {
  first <- integer(nrow(parts))
  search_from <- 1
  for (i in seq_len(nrow(parts))) {
    hits <- which(lines == parts$anchor[i])
    hits <- hits[hits >= search_from]
    if (length(hits) == 0) {
      stop("Stata anchor not found: ", parts$anchor[i])
    }
    first[i] <- hits[1]
    search_from <- hits[1] + 1
  }
  parts$first <- first
  parts$last <- c(first[-1] - 1, length(lines))
  parts
}

stata_lines <- readLines(stata_file, warn = FALSE, encoding = "UTF-8")
stata_parts <- locate_stata_parts(stata_lines, stata_parts)

# Print one or more parts as Stata code blocks, each headed by its line
# numbers in the .do file. Use in a chunk with output: asis.
show_stata <- function(ids) {
  for (id in ids) {
    part <- stata_parts[stata_parts$id == id, ]
    if (nrow(part) != 1) {
      stop("Unknown Stata part: ", id)
    }
    body <- stata_lines[part$first:part$last]
    # Drop blank lines at the end of a part
    while (length(body) > 0 && trimws(body[length(body)]) == "") {
      body <- body[-length(body)]
    }
    cat(
      "\n*", part$title, " (lines ", part$first, "–", part$first + length(body) - 1,
      " of the .do file)*\n\n```stata\n",
      sep = ""
    )
    cat(body, sep = "\n")
    cat("\n```\n\n")
  }
}
