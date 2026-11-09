# AGENTS.md

Guidance for everyone working on this repository: the maintainer, collaborators, and any AI agents working for them. It covers how to work here: conventions, data handling, and checks.

Four places divide the documentation:

| Where | Covers |
|---|---|
| `analysis-plan.md` | Study design and statistics: what the analysis is meant to do |
| `ARCHITECTURE.md` | Code structure: scripts, data flow, and the objects passed between them |
| `AGENTS.md` (this file) | How to work in the repository: rules, conventions, and checks |
| [GitHub Issues](https://github.com/VagishHemmige/Harris-Health-RUSH/issues) | Open scientific decisions (decision issues), gaps between code and plan, and known defects |

Read the three files before making changes. Each covers its own topic and does not repeat the others.

## The project

This repository is a teaching resource built on a published retrospective cohort study: the Routine Universal Screening for HIV (RUSH) program of opt-out HIV testing with service linkage in the Ben Taub and Lyndon B. Johnson emergency departments of Harris Health System, Houston, and its effect on engagement in care, retention in care and viral suppression among patients already diagnosed with HIV, compared before and after an emergency department visit in 2009 to 2012, and against a pre-RUSH era (J Acquir Immune Defic Syndr 2015, doi:10.1097/QAI.0000000000000578). The original analysis was one Stata .do file of January 2015, kept in `stata/` with only its local folder path removed. This version rebuilds it in R as labelled scripts and explains each step on a Quarto website. The code is shown, not run on the study data here: the data are not in the repository, and the site's results, including its versions of the article's tables, are the published article's values, not outputs of this code.

- **`analysis-plan.md` is the source of truth for design.** Code implements the plan; it does not redefine it. If code needs to depart from the plan, open a decision issue, or comment on the existing one, for the deciders. Once a decider has settled it (in the issue, or relayed there in their exact words), the plan change and the code can go in the same pull request, which closes the issue.
- **Deciders.** Scientific decisions are made by Vagish Hemmige, in any area. Anyone may open or contribute to a decision issue; only a decider settles it (see "Working with agents").
- **Departures from the original analysis.** The original Stata code and the paper are the record of what was done. When the R code differs from the Stata code, or the Stata code from the paper, do not silently copy it or silently fix it: the site's "Differences from the original code" page records every difference, and choices between the paper and the code go to a decision issue.
- **The original code is a record.** `stata/RUSH_analysis_2015-01-15.do` is not edited, except to remove private information (a local path, a name that is not an author's); each such change is listed in its header.
- **This is an analysis project, not an R package.** Do not add DESCRIPTION, NAMESPACE, roxygen, or pkgdown infrastructure, or apply CRAN conventions.
- **Default branch:** `main`, protected. Changes reach it only through pull requests.

## Data sources

| Source | Accessed through | Documentation | Package maintained by | Access | What AI agents may do |
|---|---|---|---|---|---|
| De-identified RUSH cohort workbook (`Deidentified RUSH data 1-15-2015.xlsx`): sheet "Study Cohort", one row per previously diagnosed patient with a RUSH-era index visit; sheet "Controls", one row per patient with a pre-RUSH visit | `R/01_import_data.R` (`readxl::read_excel()`), from the folder named by `RUSH_DATA_DIR` | The published article's Methods; no data dictionary is in the repository | No package | Restricted (Harris Health System patient data, de-identified) | None: code, documentation and synthetic data only |
| Years from HIV diagnosis to the index visit (`Deidentified Years_After_DX 1-15-2015.xlsx`, sheet "Study Cohort") | `R/01_import_data.R` (`readxl::read_excel()`), from the same folder | The published article's Methods | No package | Restricted (as above) | None: code, documentation and synthetic data only |

To allow more for a source, a person changes its row through a pull request, naming what permits it and exactly what agents may do. Agents never propose or make this change themselves, and nothing in personal instructions can grant it.

**For a source with no package:** the loading script records where the file comes from, its date, and any processing. Verify its columns against what the paper describes; never infer a column's meaning from its name. The outcome columns arrive already derived (0 or 1, or a viral load result as text): how they were derived from the medical record and CPCDMS visit data is described only by the article.

## R scripts

- **Running the analysis.** Only an authorized person runs `R/main.R`, from the project root, with the data folder set in `.Renviron`. It sources the other scripts in dependency order; `ARCHITECTURE.md` describes what each does.
- **One job per script.** When you add a definitive script, source it from `main.R` at the right point and update `ARCHITECTURE.md` in the same pull request.
- **Header comment.** Each script begins with what it does, which objects it needs from earlier scripts, and what it produces.
- **Constants and libraries** live in `R/setup.R`. Other scripts do not call `library()`, re-source `setup.R`, or hard-code values. File and sheet names, expected columns, category orders, race code lists and the models the original code skipped are named constants there.
- **Helper functions** go in `R/functions.R`. A function takes its inputs as arguments and does not read global objects.
- **No side effects outside the project.** No `setwd()`, absolute paths, or writes outside the project. The data are located only through `RUSH_DATA_DIR`. The scripts write no files; results stay as objects in the session.
- **No results in the repository.** No script, comment, commit message, issue, pull-request text or review report contains counts or values from the study data beyond what the published article reports. Numbers quoted from the article are public and may be cited, with their location in the article.

## Synthetic data and tests

- `tests/synthetic/build_synthetic_data.R` writes random workbooks with the expected sheets, columns and value formats; nothing in them comes from the study. `tests/synthetic/run_pipeline.R` runs `R/main.R` on them and checks the helpers against results known from theory (the conditional odds ratio of paired data, the within-category odds ratio, the Wald and McNemar statistics).
- `.github/workflows/test-pipeline.yml` runs it on every pull request. Agents may run it too.
- Synthetic values are invented labels where the real ones are unknown (for example "Race A"); they must never be presented as the study's categories.

## Chunk labels

Scripts are divided into labelled sections so Quarto pages can show the code without copying it:

```r
## ---- <step-name>
```

- Every logical step has a label. A label covers the code from its line to the next label.
- Labels are kebab-case, describe the step, and are **unique across all scripts**; a duplicate silently picks up the wrong code.
- Search the `.qmd` files before renaming or deleting a label.
- Labels are comments and never change what the code does.

## Quarto website

The site documents the code. It does not run the analysis.

- `_quarto.yml`, `index.qmd`, `404.qmd` and `styles.css` sit at the root; `quarto/` has one page per definitive script, named after it. The `custom-callout` extension in `_extensions/` provides the `Rcode`, `Results` and `Pending` callouts.
- Each page loads its script with `knitr::read_chunk("../R/<script>.R")` in a hidden chunk and shows each labelled section with `#| eval: false`, with prose explaining what the section does and why. Pages link to the script on GitHub and to the scripts it depends on.
- **Visual cues.** Each kind of page has one icon and colour, set by its sidebar entry's `icon:` in `_quarto.yml` and coloured by `styles.css`:

  | Kind | Sidebar icon | Colour | Callout |
  |---|---|---|---|
  | Background | `book` | blue | — |
  | Study design and methods | `diagram-3` | orange | — |
  | R code | `laptop` | pink | `Rcode` (💻) |
  | Results | `clipboard-data` | green | `Results` (📊) |
  | Project documents | `file-earmark-ruled` | grey | — |

  The original Stata code page is R code (it is code). The differences page is study design and methods. The published results page is results, and its notice says the numbers are quoted from the article, not produced by this code.
- **Published tables** are shown as tables rebuilt from the values the article prints: the values are typed into `quarto/published/table_1.csv` and `tables_2_to_4.csv`, exactly as printed, and formatted by the same functions `R/tables.R` uses on the data. Each table cites its place in the article and says it is not output of this code. Values that look inconsistent are kept as printed and noted, never corrected. The journal's own typeset tables and Figure 1 are not copied; they need the publisher's permission (RightsLink), after which the page notice states it.
- **Provisional content.** `quarto/_pending.lua` highlights in yellow every paragraph that starts with **Decision pending:**, every paragraph of the plan's page that contains the plan's **Open:** marker, and any `[text]{.pending}`, and adds a ⏳ `Pending` callout to pages that have a mark. Mark only what an open decision could change, link its decision issue, and remove the mark when the decision is settled.
- **Never evaluate analysis code in the site.** The only chunks that run are the hidden `read_chunk()` imports, the Architecture page's reader of `ARCHITECTURE.md`, the original-code page's reader of the `.do` file, which reads that file as text, and the published-results page's chunks, which read the two published-values CSVs and format them with the table functions in `R/functions.R`. None reads study data or fits a model.
- The site renders to `docs/`, which is not committed. `.github/workflows/publish.yml` renders it on every pull request as a check and, on each push to `main`, publishes it to the `gh-pages` branch, which GitHub Pages serves. When you add a script, add its page and sidebar entry in the same pull request.
- **Prose must match the code.** If a change alters what a section does, update its explanation in the same pull request.

## Outputs

- `R/tables.R` writes HTML copies of Tables 1 to 4 to `tables/`, which is gitignored: tables made from the study data are not published. `figures/` is gitignored too, in case a figure is added.
- `README.Rmd` is the source of `README.md`; edit the `.Rmd` and re-knit.

## Restricted data (applies to everyone)

- **Never commit data**, extracts, saved objects, Stata logs, or outputs made from the study data. Check `.gitignore` before adding a new output type.
- **AI agents never read the study data.** Anything an agent opens, prints, or summarizes is sent to the model provider. Agents do not open the data files, the 2015 Stata log, or outputs made from them. Agents write and review code and run it on synthetic data; only an authorized person runs it on the study data.
- **Credentials.** Never commit keys, tokens, or `.Renviron`.

## Coding conventions

- **Pre and post.** `post_visit` is 0 for the period before the index visit and 1 for the period after it, whatever the outcome's window (analysis-plan.md section 7); `rush_era` is 1 for the RUSH-era visit and 0 for the pre-RUSH visit.
- **Conditional logistic regression** is `survival::clogit()` through `fit_conditional_logit()`, with one stratum per patient and the exact conditional likelihood. In the RUSH-era models a characteristic, which cannot change within a patient there, enters only through its interactions with `post_visit`. The era models use the full factorial (`post_visit * rush_era * characteristic`), because a characteristic taken from each era's sheet can differ between a patient's visits.
- **Category order and reference levels** match the original code: text categories are numbered as Stata's `encode` numbered them (`encode_like_stata()`), so the first level is Stata's base level. Do not reorder levels in the models without a decision issue.
- **Race codes.** The two workbook sheets number race values separately. Race groups are made from each sheet's own codes, and only the groups, never the codes, are used once the eras are stacked.
- **Reported quantities** (odds ratios within a category, joint interaction tests) come from the named functions in `R/functions.R`, which are checked on synthetic data.
- **Package APIs:** read the help page and NEWS for the installed version before calling a package; don't write calls from memory.
- **Style:** readable tidyverse code, following the tidyverse style guide: dplyr pipelines (`|>`), one step per line; `case_when()` rather than nested `ifelse()`; models, rows and columns selected by name, never by position; repeated fits as for-loops over names into named lists. Style changes go in their own commits.

## Checks for any change

- **Plan:** does the change implement the plan as written, and does it leave every open decision open?
- **Departures:** does every difference from the original Stata code appear on the differences page, and is each either a correction of an error or linked to a decision issue?
- **Models:** is each model fitted within patient strata; does every interaction test find its coefficients (`Rscript tests/synthetic/run_pipeline.R` passes)?
- **Reported numbers:** does site prose say what the code computes, and is every number quoted from the article cited to its place there?
- **Structure:** is `ARCHITECTURE.md` current; are chunk labels unique and found by their pages; does every definitive script have a page and a sidebar entry with its kind's icon; does any site chunk run analysis code?
- **Disclosure:** search changed files, commit messages and pull-request text for counts or values from the study data that the article does not report.

## Working with agents

- **Autonomy.** The person directing an agent decides how much it may do, and may give it stricter rules of their own; those take precedence. By default an agent may edit code and documentation, run the synthetic tests, render the site, and commit to a branch and open a pull request.
- **Decisions.** Anyone, including an agent, may add options and evidence to a decision issue. A decider states the choice in the issue. Agents implement decisions; they never choose among options on their own, in the issue, the plan, or the code.
- **Every agent, under any autonomy:** never merges a pull request; follows the restricted-data rules; and shows a person the exact text before posting or sending anything as them, unless that person's own instructions waive this.
- **Pull requests opened by an agent** are focused. Each says which plan section or issue it addresses and what was checked, and how.
- **References:** cite the source documentation and literature used, in `references.bib`.
