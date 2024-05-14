# post_surgery_opioid_use
Code related to new opioid use after being prescribed opioids for surgery

## Causal question:
To what extent does an additive increased shift in opioid a) dose (in MME), b) days supplied, and c) days of continuous use increase risk of i) OUD, ii) opioid overdose, and iii) MOUD initiation over the 2 years following surgery?

## Cohort definition: 

Inclusion criteria:
  - age 18-64
  - had a surgery using the ICD codes given by Lisa
  - had an opioid prescribed in the range of 30 days prior to surgery to 14 days after surgery discharge

Exclusion criteria"
- history of OUD *or* overdose (any) *or* MOUD 6 months prior to surgery up to the discharge date (as a sensitivity analysis can also make this 12 months)
- pregnant
- dual eligible
- ineligible state (not in the states we are supposed to have data on)
- cancer diagnosis
- not continuously enrolled for 6 months prior to surgery date

## Observed data:
We assume observed data $\mathbf{O}=(\mathbf{W}, \mathbf{A}, \Delta_1, \Delta_1 Y_1, ..., \Delta_{24}, \Delta_{24} Y_{24})$, where: 
$\mathbf{W}$ represents the covariates, measured during the 6 months prior to surgery (i.e., during the washout period); 
$\mathbf{A}$ represent prescription opioid a) dose (in MME), b) days supplied, and c) days of continuous use, measured during 30 days prior to surgery up to 14 days after surgery discharge; 
$\Delta_t$ represents an indicator of remaining uncensored at month t following surgery discharge (24 months for the primary analysis); surgery discharge is time 0;
$Y_t$ represents the outcome of i) OUD, ii) opioid overdose, and iii) MOUD initiation at time t, which is observed among those who remain uncensored. 

** Note: If there is too much loss to follow up, we will need to shorten T. Can consider T=12 months. **


Opioids relevant to these beneficiaries and their surgeries are collected <a href="https://github.com/CI-NYC/post_surgery_opioid_use/blob/main/R/01_exclusions/01_02_include_opioids.R#L85-L93">here</a>

| Exposure               | Definition |
|------------------------|------------|
| MME                    | MME dose conversion and mean daily dose of MME are defined <a href="https://github.com/CI-NYC/medicaid_treatment_trends/blob/main/code/01_opioid_outcomes/01_02_mean_daily_dose_mme.R">here</a> using the function "calculate_mean_daily_dose". |
| Days supplied          | Relevant opioids from the OTL and RXL files are gathered <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/mediation_unsafe_pain_mgmt/01_create_mediators/01_mediator_opioid_pain_rx.R">here</a> <p> OTL and RXL opioids are then combined into the same dataframe, and days supplied code can be adapted from <a href="https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/01_create_mediators/31_mediator_proportion_days_covered.R">here</a> <p> We are interested in total days supplied, rather than the proportion. So, the only change is to not divide by the length of the time interval.|
| Days of continuous use | This can be done very similarly to days supplied. Except, instead of summing up all days of supply for opioids, I will count up and record each separate period of opioid use. <p> For every beneficiary, this produces a vector for the lengths of all periods of continuous use. |

| Outcome | Definition |
|---------|------------|
| OUD     | The dates of all OUD diagnosis codes for the cohort are identified <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/06_define_OUD_components/define_oud_hillary.R">here</a> and is called `hillary`. <p> I need to copy the original code up until L#109 because we need to store all OUD dates, not just the first occurrence (which is what the original code did). The reason we may be interested in all OUD dates is because there may be a case where a person has a very early OUD date, and it is not recent enough to exclude them from the study. They may also have a later OUD date, which is an outcome of interest. But because the original code only recorded the first OUD date, any future OUDs would be masked from us. <p> Whether the date of an OUD exists in the follow-up period needs to be defined. <p> Then, for those who have an OUD diagnosis within the follow-up period, the earliest date of an OUD diagnosis needs to be recorded. |
| OD      | OD (called `poison`) will follow the same steps as OUD. Only the diagnosis codes will change. The definition for OD can be found <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/06_define_OUD_components/define_oud_poison.R">here</a> |
| MOUD (met)   | defined <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/create_cohort/scripts/06_define_OUD_components/define_moud_met.R">here</a>|
| MOUD (nal)   | defined <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/create_cohort/scripts/06_define_OUD_components/define_moud_nal.R">here</a>|
| MOUD (bup)   | Defined <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/06_define_OUD_components/define_moud_bup.R#L22">here</a> <p> best_list.rds is required and found at disability/projects/define_moud/input/best_list.rds | 

## Estimand 
We estimate the effect of each of the opioid exposure variables on each of the outcome variables, adjusting for covariates, holding other opioid exposure variables at their observed levels. Using the notation given above, this effect can be written: 
$E(Y_T^{d_n(\mathbf{A}), \Delta=1} - Y_T^{\mathbf{A}, \Delta=1}),$ where $E(Y_T^{\mathbf{A}, \Delta=1})$ denotes the expected value of the counterfactual outcome had the set of prescription opioid variables ($\mathbf{A}$) not been intervened on (i.e., remained as observed) and had no one been censored, and where $E(Y_T^{d_n(\mathbf{A}), \Delta=1})$ denotes the expected value of the counterfactual outcome had the particular opioid variable $A_n$ been intervened on as dictated by the function $d_n(\mathbf{A})$ but the remaining opioid variables stayed as observed and had no one been censored.

TO DO:
- look at the distributions of $A_1, A_2, A_3$ and decide what shift intervention makes sense.

