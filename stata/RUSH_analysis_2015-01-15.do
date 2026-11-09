//Original Stata analysis for the RUSH study (JAIDS 2015, doi:10.1097/QAI.0000000000000578).
//Kept as the record of what was done. Changes from the 2015 file: the local folder path in
//the cd command is removed, and line endings are Unix. Nothing else is changed.
//The R port in R/ is the version explained on the website.

//Data analysis for RUSH study
//January 2015
//Vagish Hemmige

//First, we set up our file
clear
set more off
set scheme lean1
global DateOfAnalysis 1-15-2015
//The working directory was a folder on the analyst's computer holding the de-identified
//workbooks; its path is removed here. Set it to the folder that holds them before running.
//cd "<folder holding the de-identified workbooks>"

//Now we start logging the analysis
capture log close
log using "RUSH $DateOfAnalysis.log", replace

//We first import, clean, and then save as a data file the "Years since diagnosis" data
import excel "Deidentified Years_After_DX $DateOfAnalysis.xlsx", sheet("Study Cohort") firstrow
rename Years_between_DX_and_Index_Visit YearsSinceDxNumber
tab Category
generate YearsSinceDxCategorical=.
replace YearsSinceDxCategorical=1 if Category=="<=5"
replace YearsSinceDxCategorical=2 if Category=="6-10"
replace YearsSinceDxCategorical=3 if Category=="11-15"
replace YearsSinceDxCategorical=4 if Category=="16-20"
replace YearsSinceDxCategorical=5 if Category==">20"
label define YearsSinceDxCategorical 1 "<=5" 2 "6-10" 3 "11-15" 4 "16-20" 5 ">20"
label values YearsSinceDxCategorical YearsSinceDxCategorical
tab YearsSinceDxCategorical
drop Category
save "Years since diagnosis $DateOfAnalysis.dta", replace
clear

//We first import data from the main file from Excel and merge with the Years since diagnosis file:
import excel "Deidentified RUSH data $DateOfAnalysis.xlsx", sheet("Study Cohort") firstrow
merge 1:1 PatientNumber using "Years since diagnosis $DateOfAnalysis.dta"
tab _merge
drop _merge

//Now we clean variables and convert strings to numerics.  We drop unnecessary variables.
encode Race, generate(RACE)
drop Race
rename RACE Race
encode Gender, generate(GENDER)
drop Gender
rename GENDER Gender
encode AgeGroup, generate(Age)
drop AgeGroup
encode IndexTest_Place, generate(IndexTestPlace)
drop IndexTest_Place
drop EngageInCare_Before_Place EngageInCare_After_Place RetainedInCare_Before_Place RetainedInCare_After_Place

//We do the appropriate tabs to confirm that the results are the same as in the manuscript
tab Race
tab Gender
tab Age
tab IndexTestYear
tab YearsSinceDxCategorical
summarize YearsSinceDxNumber, detail

//We now rename variables in preparation of converting to long format, as required by clogit
rename EngageInCare_Before EngagedInCare0
rename EngagedInCare_After EngagedInCare1
rename RetainedInCare_Before RetainedInCare0
rename RetainedInCare_After RetainedInCare1
rename VL_Before VL0
rename VL_After VL1
save "RUSH data Wide Format $DateOfAnalysis.dta", replace

//We perform McNemer's test on EngagedInCare and RetainedInCare to confirm later that clogit gives the same results
mcc EngagedInCare1 EngagedInCare0
mcc RetainedInCare1 RetainedInCare0

//Now we reshape the dataset to long
reshape long EngagedInCare RetainedInCare VL, i(PatientNumber) j(Intervention)

//Now we convert the VL variable to two binary variables
//The variable "ViralLoad" treats missing values as missing
//The variable "ViralLoadMissEqFail" treats missing values as failing
tab VL
generate ViralLoad=.
replace ViralLoad=1 if VL=="Suppressed"
replace ViralLoad=0 if VL=="Not Suppressed"
tab ViralLoad, missing
generate ViralLoadMissEqFail=ViralLoad
replace ViralLoadMissEqFail=0 if ViralLoadMissEqFail==.
tab ViralLoadMissEqFail
drop VL

//We now create an AfricanAmerican variable from Race
generate AfricanAmerican=(Race==2)

//And we create a variable NewRace which lumps Asians with others and unknown
generate NewRace=.
replace NewRace=1 if Race==2
replace NewRace=2 if Race==3
replace NewRace=3 if Race==4
replace NewRace=4 if ((Race==1)|(Race==5)|(Race==6))
replace NewRace=3 if Race==7
label define NewRace 1 "Black" 2 "Hispanic" 3 "White" 4 "Asian/Other/Unknown"
label values NewRace NewRace
tab NewRace Race

//We generate a variable RUSHEra for later use
generate RUSHEra=1
generate HIV_tested_During_Visit=1
drop IndexTestPlace 

save "RUSH data Long Format $DateOfAnalysis.dta", replace

//First, we set loops to run the crosstabs for the raw analysis:
foreach Outcome in EngagedInCare RetainedInCare ViralLoad ViralLoadMissEqFail {
foreach Predictor in IndexTestYear Race Gender Age AfricanAmerican NewRace YearsSinceDxCategorical {
display "tab `Predictor' `Outcome' if Intervention==1, row missing"
tab `Predictor' `Outcome' if Intervention==1, row missing
display "tab `Predictor' `Outcome' if Intervention==0, row missing"
tab `Predictor' `Outcome' if Intervention==0, row missing
}
}

//We now run the conditional logistic regressions

//This set of regressions tests the efficacy of the intervention in terms of the three outcomes
clogit ViralLoadMissEqFail Intervention, group(PatientNumber) or nolog
clogit ViralLoad Intervention, group(PatientNumber) or nolog
clogit EngagedInCare Intervention, group(PatientNumber) or nolog
clogit RetainedInCare Intervention, group(PatientNumber) or nolog

//These regressions test whether the intervention's efficacy varies by year and calculates out the interaction terms.
clogit ViralLoadMissEqFail i.Intervention##i.IndexTestYear , group(PatientNumber) or nolog
testparm Intervention#IndexTestYear
lincom 1.Intervention+1.Intervention#2010.IndexTestYear, or
lincom 1.Intervention+1.Intervention#2011.IndexTestYear, or
lincom 1.Intervention+1.Intervention#2012.IndexTestYear, or

clogit ViralLoad i.Intervention##i.IndexTestYear , group(PatientNumber) or nolog
testparm Intervention#IndexTestYear
lincom 1.Intervention+1.Intervention#2010.IndexTestYear, or
lincom 1.Intervention+1.Intervention#2011.IndexTestYear, or
lincom 1.Intervention+1.Intervention#2012.IndexTestYear, or

clogit EngagedInCare i.Intervention##i.IndexTestYear , group(PatientNumber) or nolog
testparm Intervention#IndexTestYear
lincom 1.Intervention+1.Intervention#2010.IndexTestYear, or
lincom 1.Intervention+1.Intervention#2011.IndexTestYear, or
lincom 1.Intervention+1.Intervention#2012.IndexTestYear, or

clogit RetainedInCare i.Intervention##i.IndexTestYear , group(PatientNumber) or nolog
testparm Intervention#IndexTestYear
lincom 1.Intervention+1.Intervention#2010.IndexTestYear, or
lincom 1.Intervention+1.Intervention#2011.IndexTestYear, or
lincom 1.Intervention+1.Intervention#2012.IndexTestYear, or

////We now do the same for YearsSinceDiagnosisCategorical, seeing whether intervention efficacy varies by Years since diagnosis.
clogit ViralLoadMissEqFail i.Intervention##i.YearsSinceDxCategorical , group(PatientNumber) or nolog
testparm Intervention#YearsSinceDxCategorical
lincom 1.Intervention+1.Intervention#2.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#3.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#4.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#5.YearsSinceDxCategorical, or


clogit ViralLoad i.Intervention##i.YearsSinceDxCategorical , group(PatientNumber) or nolog
testparm Intervention#YearsSinceDxCategorical
lincom 1.Intervention+1.Intervention#2.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#3.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#4.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#5.YearsSinceDxCategorical, or

clogit EngagedInCare i.Intervention##i.YearsSinceDxCategorical , group(PatientNumber) or nolog
testparm Intervention#YearsSinceDxCategorical
lincom 1.Intervention+1.Intervention#2.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#3.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#4.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#5.YearsSinceDxCategorical, or

clogit RetainedInCare i.Intervention##i.YearsSinceDxCategorical , group(PatientNumber) or nolog
testparm Intervention#YearsSinceDxCategorical
lincom 1.Intervention+1.Intervention#2.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#3.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#4.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#5.YearsSinceDxCategorical, or


//We now do the same for Gender, seeing whether intervention efficacy varies by Gender.
clogit ViralLoadMissEqFail i.Intervention##i.Gender , group(PatientNumber) or nolog
lincom 1.Intervention+ 1.Intervention#2.Gender, or
clogit ViralLoad i.Intervention##i.Gender , group(PatientNumber) or nolog
lincom 1.Intervention+ 1.Intervention#2.Gender, or
clogit EngagedInCare i.Intervention##i.Gender , group(PatientNumber) or nolog
lincom 1.Intervention+ 1.Intervention#2.Gender, or
clogit RetainedInCare i.Intervention##i.Gender , group(PatientNumber) or nolog
lincom 1.Intervention+ 1.Intervention#2.Gender, or

//We now will do the same for Age:
clogit ViralLoadMissEqFail i.Intervention##i.Age , group(PatientNumber) or nolog
testparm Intervention#Age
lincom 1.Intervention+1.Intervention#2.Age, or
lincom 1.Intervention+1.Intervention#3.Age, or
lincom 1.Intervention+1.Intervention#4.Age, or
lincom 1.Intervention+1.Intervention#5.Age, or

clogit ViralLoad i.Intervention##i.Age , group(PatientNumber) or nolog
testparm Intervention#Age
lincom 1.Intervention+1.Intervention#2.Age, or
lincom 1.Intervention+1.Intervention#3.Age, or
lincom 1.Intervention+1.Intervention#4.Age, or
lincom 1.Intervention+1.Intervention#5.Age, or

clogit EngagedInCare i.Intervention##i.Age , group(PatientNumber) or nolog
testparm Intervention#Age
lincom 1.Intervention+1.Intervention#2.Age, or
lincom 1.Intervention+1.Intervention#3.Age, or
lincom 1.Intervention+1.Intervention#4.Age, or
lincom 1.Intervention+1.Intervention#5.Age, or

clogit RetainedInCare i.Intervention##i.Age , group(PatientNumber) or nolog
testparm Intervention#Age
lincom 1.Intervention+1.Intervention#2.Age, or
lincom 1.Intervention+1.Intervention#3.Age, or
lincom 1.Intervention+1.Intervention#4.Age, or
lincom 1.Intervention+1.Intervention#5.Age, or

////We now do the same for YearsSinceDiagnosisCategorical and age in the same regression, seeing whether intervention efficacy varies by Years since diagnosis.
clogit ViralLoadMissEqFail i.Intervention##i.YearsSinceDxCategorical i.Intervention##i.Age, group(PatientNumber) or nolog
testparm Intervention#YearsSinceDxCategorical
lincom 1.Intervention+1.Intervention#2.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#3.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#4.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#5.YearsSinceDxCategorical, or
testparm Intervention#Age
lincom 1.Intervention+1.Intervention#2.Age, or
lincom 1.Intervention+1.Intervention#3.Age, or
lincom 1.Intervention+1.Intervention#4.Age, or
lincom 1.Intervention+1.Intervention#5.Age, or

clogit ViralLoad i.Intervention##i.YearsSinceDxCategorical i.Intervention##i.Age, group(PatientNumber) or nolog
testparm Intervention#YearsSinceDxCategorical
lincom 1.Intervention+1.Intervention#2.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#3.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#4.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#5.YearsSinceDxCategorical, or
testparm Intervention#Age
lincom 1.Intervention+1.Intervention#2.Age, or
lincom 1.Intervention+1.Intervention#3.Age, or
lincom 1.Intervention+1.Intervention#4.Age, or
lincom 1.Intervention+1.Intervention#5.Age, or

clogit EngagedInCare i.Intervention##i.YearsSinceDxCategorical i.Intervention##i.Age, group(PatientNumber) or nolog
testparm Intervention#YearsSinceDxCategorical
lincom 1.Intervention+1.Intervention#2.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#3.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#4.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#5.YearsSinceDxCategorical, or
testparm Intervention#Age
lincom 1.Intervention+1.Intervention#2.Age, or
lincom 1.Intervention+1.Intervention#3.Age, or
lincom 1.Intervention+1.Intervention#4.Age, or
lincom 1.Intervention+1.Intervention#5.Age, or

clogit RetainedInCare i.Intervention##i.YearsSinceDxCategorical i.Intervention##i.Age, group(PatientNumber) or nolog
testparm Intervention#YearsSinceDxCategorical
lincom 1.Intervention+1.Intervention#2.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#3.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#4.YearsSinceDxCategorical, or
lincom 1.Intervention+1.Intervention#5.YearsSinceDxCategorical, or
testparm Intervention#Age
lincom 1.Intervention+1.Intervention#2.Age, or
lincom 1.Intervention+1.Intervention#3.Age, or
lincom 1.Intervention+1.Intervention#4.Age, or
lincom 1.Intervention+1.Intervention#5.Age, or

//We do the same for Race:
clogit ViralLoadMissEqFail i.Intervention##i.Race , group(PatientNumber) or nolog
//clogit ViralLoad i.Intervention##i.Race , group(PatientNumber) or nolog Didn't run because regression fails
clogit EngagedInCare i.Intervention##i.Race , group(PatientNumber) or nolog
clogit RetainedInCare i.Intervention##i.Race , group(PatientNumber) or nolog

//And for NewRace:
clogit ViralLoadMissEqFail i.Intervention##i.NewRace , group(PatientNumber) or nolog
testparm Intervention#NewRace
lincom 1.Intervention+1.Intervention#2.NewRace, or
lincom 1.Intervention+1.Intervention#3.NewRace, or
lincom 1.Intervention+1.Intervention#4.NewRace, or

clogit ViralLoad i.Intervention##i.NewRace , group(PatientNumber) or nolog
testparm Intervention#NewRace
lincom 1.Intervention+1.Intervention#2.NewRace, or
lincom 1.Intervention+1.Intervention#3.NewRace, or
lincom 1.Intervention+1.Intervention#4.NewRace, or

clogit EngagedInCare i.Intervention##i.NewRace , group(PatientNumber) or nolog
testparm Intervention#NewRace
lincom 1.Intervention+1.Intervention#2.NewRace, or
lincom 1.Intervention+1.Intervention#3.NewRace, or
lincom 1.Intervention+1.Intervention#4.NewRace, or

clogit RetainedInCare i.Intervention##i.NewRace , group(PatientNumber) or nolog
testparm Intervention#NewRace
lincom 1.Intervention+1.Intervention#2.NewRace, or
lincom 1.Intervention+1.Intervention#3.NewRace, or
lincom 1.Intervention+1.Intervention#4.NewRace, or

//And for AfricanAmerican
clogit ViralLoadMissEqFail i.Intervention##i.AfricanAmerican , group(PatientNumber) or nolog
lincom 1.Intervention+1.Intervention#1.AfricanAmerican, or
clogit ViralLoad i.Intervention##i.AfricanAmerican , group(PatientNumber) or nolog
lincom 1.Intervention+1.Intervention#1.AfricanAmerican, or
clogit EngagedInCare i.Intervention##i.AfricanAmerican , group(PatientNumber) or nolog
lincom 1.Intervention+1.Intervention#1.AfricanAmerican, or
clogit RetainedInCare i.Intervention##i.AfricanAmerican , group(PatientNumber) or nolog
lincom 1.Intervention+1.Intervention#1.AfricanAmerican, or

//Now we put it all together into a final regression model including all variables, initially with the African American variable:
clogit ViralLoadMissEqFail i.Intervention##i.IndexTestYear i.Intervention##i.Gender i.Intervention##i.Age i.Intervention##i.AfricanAmerican, group(PatientNumber) or nolog
clogit EngagedInCare i.Intervention##i.IndexTestYear i.Intervention##i.Gender i.Intervention##i.Age i.Intervention##i.AfricanAmerican, group(PatientNumber) or nolog
clogit RetainedInCare i.Intervention##i.IndexTestYear i.Intervention##i.Gender i.Intervention##i.Age i.Intervention##i.AfricanAmerican, group(PatientNumber) or nolog

//Same analysis, except with the full Race variable:
clogit ViralLoadMissEqFail i.Intervention##i.IndexTestYear i.Intervention##i.Gender i.Intervention##i.Age i.Intervention##i.Race, group(PatientNumber) or nolog
clogit EngagedInCare i.Intervention##i.IndexTestYear i.Intervention##i.Gender i.Intervention##i.Age i.Intervention##i.Race, group(PatientNumber) or nolog
clogit RetainedInCare i.Intervention##i.IndexTestYear i.Intervention##i.Gender i.Intervention##i.Age i.Intervention##i.Race, group(PatientNumber) or nolog

clear


//Now we compare with the control era
//The following lengthy block of code prepares the control data for the analysis
import excel "Deidentified RUSH data $DateOfAnalysis.xlsx", sheet("Controls") firstrow
encode Race, generate(RACE)
drop Race
rename RACE Race
encode Gender, generate(GENDER)
drop Gender
rename GENDER Gender
encode AgeGroup, generate(Age)
drop AgeGroup
drop IndexTest_Place
drop EngageInCare_Before_Place EngageInCare_After_Place RetainedInCare_Before_Place RetainedInCare_After_Place
tab Race
tab Gender
tab Age
rename EngageInCare_Before EngagedInCare0
rename EngagedInCare_After EngagedInCare1
rename RetainedInCare_Before RetainedInCare0
rename RetainedInCare_After RetainedInCare1
rename VL_Before VL0
rename VL_After VL1
reshape long EngagedInCare RetainedInCare VL, i(PatientNumber) j(Intervention)
tab VL
generate ViralLoad=.
replace ViralLoad=1 if VL=="Suppressed"
replace ViralLoad=0 if VL=="Not Suppressed"
tab ViralLoad, missing
generate ViralLoadMissEqFail=ViralLoad
replace ViralLoadMissEqFail=0 if ViralLoadMissEqFail==.
tab ViralLoadMissEqFail
drop VL
generate AfricanAmerican=(Race==2)
generate NewRace=.
replace NewRace=1 if Race==2
replace NewRace=2 if Race==3
replace NewRace=3 if Race==5
label values NewRace NewRace
tab NewRace Race
label define NewRace 1 "Black" 2 "Hispanic" 3 "White" 4 "Asian/Other/Unknown"
label values NewRace NewRace
replace NewRace=4 if ((Race==1)|(Race==4))
tab NewRace Race
generate RUSHEra=0
save "Pre RUSH Control data Long Format $DateOfAnalysis.dta", replace

//Now we do the analysis to see if the intervention in the RUSH era is more effective than the intervention in the Pre-RUSH ERA

//First, we merge the two datasets and sort
use "RUSH data Long Format $DateOfAnalysis.dta", clear
append using "Pre RUSH Control data Long Format $DateOfAnalysis.dta"
sort PatientNumber RUSHEra Intervention

//We xtset the data to do random effects regression
xtset PatientNumber

//We save the data set for later:
save "Merged Long Format $DateOfAnalysis.dta", replace


//We run fixed effects regression by request including all patients in RUSH as well as the pre-RUSH controls:
//The Intervention term tests whether the intervention in the Pre-Rush era was successful
//The Rush era term tests whether the pre-intervention outcome is different in the two eras
//The interaction term tests whether the impact of the intervention is different in the pre-Rush and Rush eras

clogit EngagedInCare i.Intervention##i.RUSHEra, group(PatientNumber) or nolog
clogit ViralLoadMissEqFail i.Intervention##i.RUSHEra, group(PatientNumber) or nolog
clogit ViralLoad i.Intervention##i.RUSHEra, group(PatientNumber) or nolog
clogit RetainedInCare i.Intervention##i.RUSHEra, group(PatientNumber) or nolog

//Now we include the interactions with each of the variables of interest (this becomes ridiculously complicated, but is included for completeness)
//Interacting with test year doesn't make sense because test year doesn't overlap between the two eras (obviously)

//We look at interactions by Gender:
clogit EngagedInCare i.Intervention##i.RUSHEra##i.Gender, group(PatientNumber) or nolog
testparm 1.Intervention#2.Gender 1.RUSHEra#2.Gender 1.Intervention#1.RUSHEra#2.Gender

clogit ViralLoadMissEqFail i.Intervention##i.RUSHEra##i.Gender, group(PatientNumber) or nolog
testparm 1.Intervention#2.Gender 1.RUSHEra#2.Gender 1.Intervention#1.RUSHEra#2.Gender

clogit ViralLoad i.Intervention##i.RUSHEra##i.Gender, group(PatientNumber) or nolog
testparm 1.Intervention#2.Gender 1.RUSHEra#2.Gender 1.Intervention#1.RUSHEra#2.Gender

clogit RetainedInCare i.Intervention##i.RUSHEra##i.Gender, group(PatientNumber) or nolog
testparm 1.Intervention#2.Gender 1.RUSHEra#2.Gender 1.Intervention#1.RUSHEra#2.Gender

//We look at interactions by Age:
clogit EngagedInCare i.Intervention##i.RUSHEra##i.Age, group(PatientNumber) or nolog
testparm i.Intervention#i.Age i.RUSHEra#i.Age i.Intervention#i.RUSHEra#i.Age

clogit ViralLoadMissEqFail i.Intervention##i.RUSHEra##i.Age, group(PatientNumber) or nolog
testparm i.Intervention#i.Age i.RUSHEra#i.Age i.Intervention#i.RUSHEra#i.Age

clogit ViralLoad i.Intervention##i.RUSHEra##i.Age, group(PatientNumber) or nolog
testparm i.Intervention#i.Age i.RUSHEra#i.Age i.Intervention#i.RUSHEra#i.Age

clogit RetainedInCare i.Intervention##i.RUSHEra##i.Age, group(PatientNumber) or nolog
testparm i.Intervention#i.Age i.RUSHEra#i.Age i.Intervention#i.RUSHEra#i.Age

//We look at interactions by NewRace:
clogit EngagedInCare i.Intervention##i.RUSHEra##i.NewRace, group(PatientNumber) or nolog
testparm i.Intervention#i.NewRace i.RUSHEra#i.NewRace i.Intervention#i.RUSHEra#i.NewRace

//clogit ViralLoadMissEqFail i.Intervention##i.RUSHEra##i.NewRace, group(PatientNumber) or nolog
//testparm i.Intervention#i.NewRace i.RUSHEra#i.NewRace i.Intervention#i.RUSHEra#i.NewRace

clogit ViralLoad i.Intervention##i.RUSHEra##i.NewRace, group(PatientNumber) or nolog
testparm i.Intervention#i.NewRace i.RUSHEra#i.NewRace i.Intervention#i.RUSHEra#i.NewRace

//Retained in care failed due to data sparseness
//clogit RetainedInCare i.Intervention##i.RUSHEra##i.NewRace, group(PatientNumber) or nolog
//testparm i.Intervention#i.NewRace i.RUSHEra#i.NewRace i.Intervention#i.RUSHEra#i.NewRace

//In comparison, now we run random-effects regressions:

xtlogit EngagedInCare i.Intervention##i.RUSHEra, or nolog
xtlogit ViralLoadMissEqFail i.Intervention##i.RUSHEra, or nolog
xtlogit ViralLoad i.Intervention##i.RUSHEra, or nolog
xtlogit RetainedInCare i.Intervention##i.RUSHEra, or nolog


//Now, we count the number of observations for each patient and only keep if they have a Pre-Rush Control
by PatientNumber: egen NumObs=count( PatientNumber )
tab NumObs
keep if NumObs==4

//Now, we actually run the conditional logistic regression, with a variable for intervention and a second variable for RUSH era, as well as an interaction
//The Intervention term tests whether the intervention in the Pre-Rush era was successful
//The Rush era term tests whether the pre-intervention outcome is different in the two eras
//The interaction term tests whether the impact of the intervention is different in the pre-Rush and Rush eras

clogit EngagedInCare i.Intervention##i.RUSHEra, group(PatientNumber) or nolog
clogit ViralLoadMissEqFail i.Intervention##i.RUSHEra, group(PatientNumber) or nolog
clogit ViralLoad i.Intervention##i.RUSHEra, group(PatientNumber) or nolog
clogit RetainedInCare i.Intervention##i.RUSHEra, group(PatientNumber) or nolog


//Now we include the interactions with each of the variables of interest (this becomes ridiculously complicated, but is included for completeness)
//Interacting with test year doesn't make sense because test year doesn't overlap between the two eras (obviously)

//We look at interactions by Gender:
clogit EngagedInCare i.Intervention##i.RUSHEra##i.Gender, group(PatientNumber) or nolog
testparm 1.Intervention#2.Gender 1.RUSHEra#2.Gender 1.Intervention#1.RUSHEra#2.Gender

clogit ViralLoadMissEqFail i.Intervention##i.RUSHEra##i.Gender, group(PatientNumber) or nolog
testparm 1.Intervention#2.Gender 1.RUSHEra#2.Gender 1.Intervention#1.RUSHEra#2.Gender

clogit ViralLoad i.Intervention##i.RUSHEra##i.Gender, group(PatientNumber) or nolog
testparm 1.Intervention#2.Gender 1.RUSHEra#2.Gender 1.Intervention#1.RUSHEra#2.Gender

clogit RetainedInCare i.Intervention##i.RUSHEra##i.Gender, group(PatientNumber) or nolog
testparm 1.Intervention#2.Gender 1.RUSHEra#2.Gender 1.Intervention#1.RUSHEra#2.Gender

//We look at interactions by Age:
clogit EngagedInCare i.Intervention##i.RUSHEra##i.Age, group(PatientNumber) or nolog
testparm i.Intervention#i.Age i.RUSHEra#i.Age i.Intervention#i.RUSHEra#i.Age

clogit ViralLoadMissEqFail i.Intervention##i.RUSHEra##i.Age, group(PatientNumber) or nolog
testparm i.Intervention#i.Age i.RUSHEra#i.Age i.Intervention#i.RUSHEra#i.Age

clogit ViralLoad i.Intervention##i.RUSHEra##i.Age, group(PatientNumber) or nolog
testparm i.Intervention#i.Age i.RUSHEra#i.Age i.Intervention#i.RUSHEra#i.Age

clogit RetainedInCare i.Intervention##i.RUSHEra##i.Age, group(PatientNumber) or nolog
testparm i.Intervention#i.Age i.RUSHEra#i.Age i.Intervention#i.RUSHEra#i.Age

//We look at interactions by NewRace:
clogit EngagedInCare i.Intervention##i.RUSHEra##i.NewRace, group(PatientNumber) or nolog
testparm i.Intervention#i.NewRace i.RUSHEra#i.NewRace i.Intervention#i.RUSHEra#i.NewRace

clogit ViralLoadMissEqFail i.Intervention##i.RUSHEra##i.NewRace, group(PatientNumber) or nolog
testparm i.Intervention#i.NewRace i.RUSHEra#i.NewRace i.Intervention#i.RUSHEra#i.NewRace

clogit ViralLoad i.Intervention##i.RUSHEra##i.NewRace, group(PatientNumber) or nolog
testparm i.Intervention#i.NewRace i.RUSHEra#i.NewRace i.Intervention#i.RUSHEra#i.NewRace

clogit RetainedInCare i.Intervention##i.RUSHEra##i.NewRace, group(PatientNumber) or nolog
testparm i.Intervention#i.NewRace i.RUSHEra#i.NewRace i.Intervention#i.RUSHEra#i.NewRace

//And for comparison, we again run random-effects regressions:

xtlogit EngagedInCare i.Intervention##i.RUSHEra, or nolog
xtlogit ViralLoadMissEqFail i.Intervention##i.RUSHEra, or nolog
xtlogit ViralLoad i.Intervention##i.RUSHEra, or nolog
xtlogit RetainedInCare i.Intervention##i.RUSHEra, or nolog


clear
log close
