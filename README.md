
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Status](https://img.shields.io/badge/status-teaching_resource-yellow)](#)
[![Methods](https://img.shields.io/badge/methods-published_2015-blue)](#)
[![Published](https://img.shields.io/badge/published-J_Acquir_Immune_Defic_Syndr_2015-green)](https://doi.org/10.1097/QAI.0000000000000578)
[![Data](https://img.shields.io/badge/data-synthetic_only-lightgrey)](#)
[![License](https://img.shields.io/badge/license-MIT_%2B_CC_BY_4.0-lightgrey)](LICENSE.md)
<!-- badges: end -->

# RUSH: re-linkage to HIV care after routine emergency department testing

The goal of this analysis is to determine whether previously diagnosed
people with HIV who were retested in Houston emergency departments with
routine opt-out testing and service linkage (the RUSH program) were more
often engaged in care, retained in care and virally suppressed after the
visit than before it, and whether that change was larger than around a
visit before RUSH. It uses de-identified Harris Health System data and
reproduces the published analysis (Flash et al., J Acquir Immune Defic
Syndr 2015,
[doi:10.1097/QAI.0000000000000578](https://doi.org/10.1097/QAI.0000000000000578))
as a teaching resource.

The original 2015 Stata .do file is in [`stata/`](stata/), with only a
local folder path removed. The R scripts in `R/` rebuild it and have
been run on synthetic data only.

The analysis website, which explains the methods and shows the code step
by step, is at: <https://vagishhemmige.github.io/Harris-Health-RUSH/>

## Documentation

- [`analysis-plan.md`](analysis-plan.md): what the analysis does (the
  published methods)
- [`ARCHITECTURE.md`](ARCHITECTURE.md): how the code is organized
- [`AGENTS.md`](AGENTS.md): how to work in this repository
- [`REPOSITORY-BLUEPRINT.md`](REPOSITORY-BLUEPRINT.md): the standard
  repository layout
- [Issues](https://github.com/VagishHemmige/Harris-Health-RUSH/issues):
  open decisions and known gaps

## Running the analysis

Requires authorized access to the de-identified workbooks, which are not
in this repository. Set `RUSH_DATA_DIR` in `.Renviron` to the folder
that holds them, restart R, and run `R/main.R` from the project root.
Needs the packages attached in `R/setup.R`.

To run the code on synthetic data instead:
`Rscript tests/synthetic/run_pipeline.R` (also needs writexl).

## License and citation

Code under the MIT License; written content under CC BY 4.0 (see
[`LICENSE.md`](LICENSE.md)). If you use this material, please cite the
article above; GitHub’s “Cite this repository” button gives the
reference from [`CITATION.cff`](CITATION.cff).

<!-- Results: add only when the figures and tables are cleared for public release. -->
