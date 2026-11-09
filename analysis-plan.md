# Analysis plan: Routine opt-out HIV testing and re-linkage to care for previously diagnosed patients in Houston emergency departments

Status: draft. The plan records the analysis as it was published [@flash2015rush]; it is not a plan for new analysis.

## Scope and reporting framework

This document is the authoritative specification of the study's objectives, population, definitions and statistical methods. `ARCHITECTURE.md` documents the implementation, `AGENTS.md` the working conventions, and GitHub Issues the open decisions and implementation status. Neither file supersedes this plan.

The study is a retrospective cohort study of routinely collected health data: laboratory, electronic medical record and Ryan White visit records. Reporting follows STROBE and RECORD [@vonelm2007strobe; @benchimol2015record]. The reporting map at the end links each relevant checklist item to a section of this plan. Every method below is taken from the published article [@flash2015rush] or from the original Stata code of 15 January 2015 that produced its results (`stata/RUSH_analysis_2015-01-15.do`). Where the two differ, the plan says so.

## 1. Objective

To determine whether previously diagnosed people with HIV who were tested again in an emergency department running routine opt-out HIV testing with embedded service linkage workers, the Routine Universal Screening for HIV (RUSH) program, were more often engaged in care, retained in care and virally suppressed after the visit than before it [@flash2015rush].

- **Primary:** the change from before to after the RUSH-era index visit in each of three outcomes (section 7).
- **Secondary:** whether the change differs by sex, race and ethnicity, age, year of the visit and years since diagnosis (section 8.4); and whether the change around a RUSH-era visit is larger than the change around a visit by the same patients in the pre-RUSH era (section 8.6).
- **Hypothesis**, stated in the article: repeated screening by the program facilitates linkage or re-linkage to HIV care for previously diagnosed patients.

## 2. Design

A retrospective cohort with paired before-and-after measurement: each patient's outcomes in a period just before an emergency department visit are compared with the same outcomes just after it, so each patient serves as their own comparison. A historical comparison of the same kind, around an earlier visit by a subset of the same patients in the era before RUSH, tests whether the RUSH-era change is larger than the change around an emergency department visit without the program [@flash2015rush].

The design does not identify a causal effect of the program: the article notes the observational data and historical controls, and temporal changes in treatment guidelines and attention to retention [@flash2015rush] (section 11).

## 3. Data source

- **Setting:** the Ben Taub General Hospital and Lyndon B. Johnson General Hospital emergency departments of Harris Health System, Houston, Texas, the county's publicly funded health system [@flash2015rush].
- **HIV test records and results:** electronic laboratory databases of the RUSH program, 2009 to 2012. Positive results are matched by the City of Houston Department of Health and Human Services against local and state surveillance to identify previously diagnosed cases.
- **Demographics, visits and viral loads:** the Harris Health electronic medical record. Viral loads are available only for tests done within Harris Health.
- **Visits at other Ryan White funded clinics** in the Houston Eligible Metropolitan Area: the Centralized Patient Care Data Management System (CPCDMS), which records the date and place of Ryan White funded visits only.
- **Analysis files:** the cohort and outcomes were assembled before the Stata analysis into two de-identified workbooks, dated 15 January 2015: `Deidentified RUSH data 1-15-2015.xlsx` (sheets "Study Cohort" and "Controls") and `Deidentified Years_After_DX 1-15-2015.xlsx`. Each holds one row per patient, with the outcomes already derived for the periods before and after the visit. The code that built them is not part of this repository.

## 4. Study population

The RUSH-era study cohort, in the order the article applies the steps [@flash2015rush] (Results, first paragraph; Figure 1):

1. HIV tests performed in the two emergency departments between 2009 and 2012 (the article reports 202,767).
2. Confirmed positive results (3,613), of which 2,958 were in previously diagnosed people (2,188 individuals).
3. Positive results from a visit at least one year after the person's original HIV diagnosis (2,687 results), to limit confounding by recently diagnosed patients who may not yet have had time to be engaged, retained or suppressed.
4. The first such visit per patient: the **index visit**. This gives 2,068 patients.

Every patient in the "Study Cohort" sheet is analyzed; the scripts apply no further exclusion. The merge with the years-since-diagnosis workbook keeps every patient in either file, as the original code did.

## 5. Exposure and time zero

- **Exposure:** an emergency department visit with an HIV test under RUSH, followed by service linkage (contact by a service linkage worker, counseling, help with eligibility, transport and appointments) [@flash2015rush].
- **Time zero:** the index visit.
- **Covariates**, each fixed at the index visit: sex; race and ethnicity; age group; calendar year of the index visit (2009 to 2012); and years from HIV diagnosis to the index visit, as a number and in five groups (5 or fewer, 6 to 10, 11 to 15, 16 to 20, more than 20). Because each is the same before and after the visit, each enters the models only through its interaction with the period (section 8.3).

## 6. Comparator

- **Within patient:** the period before the index visit (section 7).
- **Pre-RUSH era:** patients in the study cohort who had an emergency department visit in a four-year period before RUSH (the article's example is 2004 to 2007) whose follow-up would not extend into the RUSH era, and who had been diagnosed more than one year before that visit. If a patient had several, the visit nearest to the RUSH-era index visit is used. In that era HIV testing was at the provider's discretion, no service linkage workers worked in the emergency department, and referral to HIV care was passive [@flash2015rush]. The article reports 672 such patients. They are a subset of the study cohort and carry the same patient number in the "Controls" sheet.

## 7. Outcomes

Each outcome is measured in a window before and a window after the visit [@flash2015rush] (Methods, "Data Sources and Analysis"):

1. **Engagement in care:** at least one HIV primary care visit in the 6 months before, and in the 6 months after, the visit. The article calls this "engagement" rather than "linkage" because the patients were not newly diagnosed.
2. **Retention in care:** the HRSA definition, two HIV primary care visits at least 3 months apart in the 12 months before, and in the 12 months after, the visit.
3. **Viral suppression:** an HIV viral load below 200 copies/mL at any time in the 12 months before, and the 12 months after, the visit. Because a viral load drawn within days of the visit reflects care before it, the before window runs to 10 days after the visit, and the after window from day 11 to 12 months and 10 days after it.
   - **Primary analysis:** a patient with no viral load result in a window counts as not suppressed.
   - **Secondary analysis:** a patient with no result in a window is missing for that window. Only patients with a result in both windows inform the paired comparison.

Visits count if they appear in the Harris Health record or in CPCDMS. In the workbooks each care outcome is 0 or 1 for each window, and viral load is the text "Suppressed" or "Not Suppressed", or empty.

## 8. Statistical analysis

### 8.1 Data structure

One record per patient and window: the before and after values of each outcome form a matched pair, and in the era comparison a patient with a visit in both eras contributes two pairs.

### 8.2 Overall change

The proportion of patients with each outcome before and after the index visit, compared with McNemar's test for paired proportions [@mcnemar1947; @flash2015rush]. The original code ran McNemar's test for engagement and retention only, with the comment that it was there to confirm the conditional logistic regression (section 8.3) gives the same results. The article's Methods describe McNemar's test for all three outcomes; the code does not record where the viral suppression P value came from.

**Open:** whether the R version should add McNemar's test for viral suppression, as the article's Methods describe, or keep the original code's set (decision issue [#3](https://github.com/VagishHemmige/Harris-Health-RUSH/issues/3)).

### 8.3 Conditional logistic regression

Each outcome is modelled with conditional logistic regression, with one stratum per patient, the outcome as the dependent variable and an indicator for the period after the visit (`post_visit`) as the main term [@flash2015rush]. Conditioning on the patient removes every characteristic that does not change between the two windows. The exponentiated `post_visit` coefficient is the conditional odds ratio for the outcome after the visit compared with before it. The exact conditional likelihood is used, as Stata's `clogit` uses.

### 8.4 Differences by patient characteristics

For each characteristic in section 5, and each outcome, a conditional logistic regression with `post_visit` and its interaction with the characteristic [@flash2015rush]:

- **Test of difference:** a joint Wald test that every interaction coefficient is zero (Stata's `testparm`).
- **Effect within each category:** the odds ratio for `post_visit` in each category, from the sum of the `post_visit` coefficient and that category's interaction coefficient (Stata's `lincom`).
- **Race and ethnicity** in four groups: Black, Hispanic, White, and other (the original code labels it "Asian/Other/Unknown"); the article's Methods name the groups White non-Hispanic, Black non-Hispanic, Hispanic and other. The original code also tested race as recorded, and an African American indicator.
- **Years since diagnosis and age together:** one model with both interactions, to see whether a difference by years since diagnosis remains once age is allowed for (original code).
- **Not fitted:** the interaction of race as recorded with viral suppression (missing excluded), which the original code could not fit.

The reference category is the first in the original code's order: the earliest year (2009); 5 or fewer years since diagnosis; and, for sex, age group and race, the first value in alphabetical order of the recorded text (Stata's `encode` numbering).

### 8.5 Multivariable models

For engagement, retention and viral suppression (missing as not suppressed), a conditional logistic regression with `post_visit` and its interactions with year of visit, sex, age group and the African American indicator; and the same with race as recorded in place of the indicator (original code). The abstract's "multivariate conditional logistic regression" refers to these models; the article does not tabulate them.

### 8.6 Comparison with the pre-RUSH era

The two cohorts are stacked: up to four records per patient (before and after each era's visit). A conditional logistic regression with one stratum per patient includes [@flash2015rush]:

- `post_visit`: the change around a pre-RUSH visit;
- `rush_era`: the difference between the periods before each era's visit;
- `post_visit × rush_era`: whether the change around the visit differs between the eras, as a ratio of odds ratios. Its P value is the one the article reports for each outcome.

The original code also fitted, for sex, age group and race in four groups, the same model with every term interacted with the characteristic, testing all the interaction terms jointly; and, for comparison, a random-intercept logistic model in place of the strata (Stata's `xtlogit`, adaptive quadrature with 12 points) [@bates2015lme4]. Year of visit is not used: it cannot overlap between eras. With every patient, two three-way models could not be fitted (race groups with viral suppression, missing as not suppressed; race groups with retention) and are not fitted.

The original code fitted every era model twice: first with every patient, including those with a RUSH-era visit only (its comment says this was done "by request"), and then with only the patients with a visit in both eras.

**Open:** which of the two samples produced the P values the article reports for the era comparison (decision issue [#2](https://github.com/VagishHemmige/Harris-Health-RUSH/issues/2)).

### 8.7 Missing data

Missing viral load results are handled as in section 7.3. The care outcomes are used as recorded in the workbook; a patient missing one in a window drops out of the comparisons of that outcome. Patients missing a characteristic are left out of the models that use it.

### 8.8 Multiplicity and significance

No adjustment for multiple comparisons. P < 0.05 is significant.

### 8.9 Software

The published analysis used Stata 12 and SPSS 21 [@flash2015rush]; the Stata code is in `stata/`. The article's Tables 1 to 4 are not made by the Stata code; the R version builds them (`R/tables.R`, with gtsummary): Table 1 as counts and percentages by cohort, and Tables 2 to 4 as, for each subgroup, the numbers before and after, the odds ratio for the visit with its Wald 95% confidence interval and Wald P value from the conditional logistic regression, and the interaction test of section 8.4 across the subgroup's categories. The odds ratio for each category comes from the model with the visit-by-characteristic interaction (section 8.4, as the original code's `lincom`) and the Total row from the model with the visit alone (section 8.3), so every row of a table is on the same footing; these are the statistics the article prints (decision issue [#5](https://github.com/VagishHemmige/Harris-Health-RUSH/issues/5)). A category in which no patient's outcome got worse has an infinite odds ratio and no P value, printed "∞" and "-" as in the article's Table 4. The tables run no McNemar test. Mean age is not computed: the workbooks hold age groups only. The R version uses `survival::clogit()` [@therneau_survival] and `lme4::glmer()` [@bates2015lme4].

## 9. Sensitivity analyses

- **Viral load missing as missing** (section 7.3), against the primary analysis in which missing counts as not suppressed. It tests the assumption that a patient without a Harris Health viral load was not suppressed.
- **Era comparison with every patient or only those with both eras** (section 8.6).
- **Random-intercept models** (section 8.6), which also use differences between patients and assume the patient effect is unrelated to the visit and era.

## 10. Disclosure control

The workbooks are de-identified patient-level data of Harris Health System and are not published in this repository. Only numbers reported in the article are quoted, with their location in the article. Nothing computed from the study data is committed.

## 11. Limitations to address in reporting

As the article discusses [@flash2015rush]:

- visits at clinics outside Harris Health that are not Ryan White funded, and viral loads drawn outside Harris Health, are not seen; counting them as failures biases every outcome downward;
- a change of provider at the time of the visit could exaggerate the change if earlier care was elsewhere and unseen;
- observational data and historical controls do not support strong causal statements; changes in treatment guidelines and in attention to retention over time may confound the era comparison;
- the "other" race and ethnicity group is small.

## Reporting map

| Checklist item | Topic | Plan section |
|---|---|---|
| STROBE 4 | Study design | 2 |
| STROBE 5 | Setting, dates | 3, 4, 6 |
| STROBE 6 / RECORD 6.1–6.3 | Eligibility, selection, linkage | 3, 4, 6 |
| STROBE 7 / RECORD 7.1 | Variables, code definitions | 5, 7 |
| STROBE 8 | Data sources and measurement | 3, 7 |
| STROBE 9 | Bias | 2, 11 |
| STROBE 10 | Study size | 4 |
| STROBE 11 | Quantitative variables | 5 |
| STROBE 12 | Statistical methods | 8, 9 |
| RECORD 12.1–12.2 | Data access, cleaning, linkage | 3, 10 |
| STROBE 19 | Limitations | 11 |

## References

::: {#refs}
:::
