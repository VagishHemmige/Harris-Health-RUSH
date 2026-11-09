# Purpose: run the analysis in dependency order.
# Run from the project root in a fresh session, by a person authorized to use
# the study data. Before the first run, set RUSH_DATA_DIR in .Renviron to the
# folder holding "Deidentified RUSH data 1-15-2015.xlsx" and
# "Deidentified Years_After_DX 1-15-2015.xlsx", and restart R.
# The scripts print the checks the original Stata log showed and leave their
# results as objects in the session. The only files written are HTML copies
# of Tables 1 to 4 in tables/, which is gitignored.

source("R/setup.R")
source("R/functions.R")
source("R/01_import_data.R")         # read both cohorts; add years since diagnosis
source("R/02_build_long_data.R")     # two rows per patient; viral load and race groups
source("R/03_paired_comparisons.R")  # McNemar's tests and crosstabs
source("R/04_fit_rush_era_models.R") # conditional logistic regression, RUSH era
source("R/05_compare_eras.R")        # RUSH era against the pre-RUSH era
source("R/tables.R")                 # Tables 1 to 4 with gtsummary, to tables/
