# post_surgery_opioid_use
Code related to new opioid use after being prescribed opioids for surgery

## Causal question:
To what extent does an additive increased shift in opioid a) dose (in MME), b) days supplied, and c) days of continuous use increase risk of i) OUD, ii) opioid overdose, and iii) MOUD initiation over the 2 years following surgery?

## Cohort definition: 

Inclusion criteria:
  - age 18-64
  - had a surgery using the ICD codes given by Lisa
  - had an opioid prescribed in the range of 30 days prior to surgery to 14 days after surgery discharge

Exclusion criteria:
- history of OUD *or* overdose (any) *or* MOUD 6 months prior to surgery up to the discharge date (as a sensitivity analysis can also make this 12 months)
- pregnant
- dual eligible
- ineligible state (not in the states we are supposed to have data on)
- cancer diagnosis
- not continuously enrolled for 6 months prior to surgery date
- had an opioid prescribed between 6 months prior to surgery to 1 month prior to surgery

## Observed data:
We assume observed data $\mathbf{O}=(\mathbf{W}, \mathbf{A}, \Delta_1, \Delta_1 Y_1, ..., \Delta_{24}, \Delta_{24} Y_{24})$, where: 
$\mathbf{W}$ represents the covariates, measured during the 6 months prior to surgery (i.e., during the washout period); 
$\mathbf{A}$ represent prescription opioid a) dose (in MME), b) days supplied, and c) days of continuous use, measured during 30 days prior to surgery up to 14 days after surgery discharge; 
$\Delta_t$ represents an indicator of remaining uncensored at month t following surgery discharge (24 months for the primary analysis); surgery discharge + 14 days is time 0;
$Y_t$ represents the outcome of i) OUD, ii) opioid overdose, and iii) MOUD initiation at time t, which is observed among those who remain uncensored. 

** Note: If there is too much loss to follow up, we will need to shorten T. Can consider T=12 months. **

### Brief overview of steps:
1. Gather all SURGERIES.
2. Apply exclusion criteria to the surgeries.
3. This may leave multiple surgeries for the same beneficiaries. I will take just the first surgery for anyone who has multiple. The result is a COHORT where each beneficiary can be matched to a single surgery.
4. Exposures will be calculated for the cohort during the perioperative period.
5. Outcomes will be calculated for the cohort during the follow-up period.

### Inclusions/exclusions
Some variables indicate what a surgery needs to have to be INCLUDED in the cohort, and others indicate features of a surgery that will cause it to be EXCLUDED. A flag (0 or 1) will be created for each criteria. Regardless of whether a variable is considered an inclusion or exclusion criteria, for every variable in the following table, I will be making flags that indicate that a surgery should be removed.

| Criteria                | Definition |
|-------------------------|------------|
| Has an eligible surgery (inclusion)| Eligible surgery codes were provided in ICD-9 format. I have converted them to ICD-10 using the 2018 GEM, `2018 General Equivalence Mappings (GEMS) (ZIP)`, found at <a href="https://www.cms.gov/medicare/coding-billing/icd-10-codes/2018-icd-10-pcs-gem"</a>|
| Age 18-64 (inclusion) | Defined <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/02_clean_tafdebse.R#L69-L72">here</a> |
| Opioid prescription (inclusion) | Create a flag that indicates that the surgery DOES NOT have an ELIGIBLE opioid (prescribed during the perioperative period). |
| Opioid prescription (exclusion) | Create a flag that indicates that the surgery DOES have an INELIGIBLE opioid (prescribed between start of washout to start of perioperative period). |
| OUD (exclusion) | The dates of all OUD diagnosis codes for the cohort are identified <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/06_define_OUD_components/define_oud_hillary.R">here</a> and is called `hillary`. <p> I need to copy the original code up until L#109 because we need to store all OUD dates, not just the first occurrence (which is what the original code did). The reason we may be interested in all OUD dates is because there may be a case where a person has multiple OUD dates, but their earliest one does not fall in either washout or follow-up period. I need to know all OUD dates to be able to exclude individuals correctly and to determine who develops OUD during the follow-up period. <p> Whether the date of an OUD exists in the washout period needs to be defined. |
| OD (exclusion) | OD (called `poison`) will follow the same steps as OUD. Only the diagnosis codes will change. The definition for OD can be found <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/06_define_OUD_components/define_oud_poison.R">here</a> |
| Not continuously enrolled (exclusion) | Beneficiaries need to be continuously enrolled for the entire 6-month washout period. Whether beneficiaries remain continuously enrolled can be deduced from the file `/mnt/general-data/create_cohort/intermediate/tafdedts/nest_dts.rds` |
| Pregnant (exclusion) | Defined <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/02_clean_tafdebse.R#L277">here</a> |
| Dual eligible (exclusion) | Defined <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/02_clean_tafdebse.R#L334">here</a> |
| Cancer (exclusion) | Defined <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/02_clean_tafdebse.R#L311">here</a> |

### Confounders

| Exposure               | Definition |
|------------------------|------------|
| Age                    | Should already have this after making the age exclusion variable. Found <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/02_clean_tafdebse.R#L69-L72">here</a> |
| Sex          | Called `SEX_CD`. Can be joined from the df `joined_df.rds` located at `/mnt/general-data/disability/create_cohort/final/` |
| Type of surgery | This is already stored as LINE_PRCDR_CD in the cohort, `first_surgeries.rds`. <p>To represent this in the analysis data frame, I will need to make a column for each type of surgery (~40 types/ICD-9 codes OR ~120 ICD-10 codes), where each column is a binary indicator of whether the beneficiary had or did not have that type of surgery. <p>Beneficiaries may have undergone multiple procedures during their surgery.|
| Anxiety | Defined <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/04_define_comorbidity_vars/define_anxiety.R">here</a>. Need to make a binary indicator of whether or not there exists a claim for anxiety during the washout period. |
| Depression | Defined <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/04_define_comorbidity_vars/define_depression.R">here</a>. Need to make a binary indicator of whether or not there exists a claim for depression during the washout period. |
| Bipolar | Defined <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/04_define_comorbidity_vars/define_bipolar.R#L1">here</a>. Need to make a binary indicator of whether or not there exists a claim for bipolar disorder during the washout period. |
| Medical comorbidities | Preoperative pain disorders (back pain, neck pain, arthritis), codes from Lisa. Need a binary indicator for each. <p>Will make a column for each of back, neck, and arthritis.|
| Other substance use disorders | Alcohol abuse disorder (CCS code: 660) <p>Substance abuse disorder (CCS code: 661). Make a binary indicator for both. <p>Problem: did not identify any matches for these CCS codes. Maybe find the ICD-10 codes related to alcohol and substance abuse? |
| Smoking history | ICD-9 codes from the original paper are: 305.1; V15.82. Convert codes to ICD-10 first. |

### Exposures
Opioids relevant to these beneficiaries and their surgeries are collected <a href="https://github.com/CI-NYC/post_surgery_opioid_use/blob/main/R/01_exclusions/01_02_include_opioids.R#L85-L93">here</a>. When creating the inclusion variable for identifying beneficiaries with an opioid prescription during the perioperative period, the eligible opioids will also be saved as `opioids_for_surgery.rds` in the opioid_data folder on the server.

| Exposure               | Definition |
|------------------------|------------|
| MME                    | MME dose conversion and mean daily dose of MME are defined <a href="https://github.com/CI-NYC/medicaid_treatment_trends/blob/main/code/01_opioid_outcomes/01_02_mean_daily_dose_mme.R">here</a> using the function "calculate_mean_daily_dose". |
| Days supplied          | Relevant opioids from the OTL and RXL files are gathered <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/mediation_unsafe_pain_mgmt/01_create_mediators/01_mediator_opioid_pain_rx.R">here</a> <p> OTL and RXL opioids are then combined into the same dataframe, and days supplied code can be adapted from <a href="https://github.com/CI-NYC/disability/blob/main/projects/mediation_unsafe_pain_mgmt/01_create_mediators/31_mediator_proportion_days_covered.R">here</a> <p> We are interested in total days supplied, rather than the proportion. So, the only change is to not divide by the length of the time interval.|
| Days of continuous use | This can be done very similarly to days supplied. Except, instead of summing up all days of supply for opioids, I will count up and record each separate period of opioid use. <p> For every beneficiary, this produces a vector for the lengths of all periods of continuous use. |

For all outcomes, I will need to record the date of the first occurrence (after the start of the follow-up period)

| Censoring variable     | Definition |
|------------------------|------------|
|                        | Can deduce how long a beneficiary remains continuously enrolled after the follow-up period begins from the dateset: `/mnt/general-data/create_cohort/intermediate/tafdedts/nest_dts.rds`. <p>Allowing for a 3 month grace period where the beneficiary may exit Medicaid and return without being censored. In this case, carry forward their most recent outcome values.|

| Outcome                | Definition |
|------------------------|------------|
| OUD     | Because OUD is also an exclusion criteria, by now we should already have all the dates of OUD for our beneficiaries. <p> Whether the date of an OUD exists in the follow-up period needs to be defined. <p> Then, for those who have an OUD diagnosis within the follow-up period, the earliest date of an OUD diagnosis needs to be recorded. |
| OD      | OD will also need to be identified within the follow-up period using the same method as for OUD.|
| MOUD (met)   | Same as above, find the first occurrence of a met prescription. <p>defined <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/create_cohort/scripts/06_define_OUD_components/define_moud_met.R">here</a>|
| MOUD (nal)   | Find the first occurrence of a nal prescription. <p>defined <a href="https://github.com/CI-NYC/disability/blob/4a9cb21be99b54a53f6716281277a6821ca7352b/projects/create_cohort/scripts/06_define_OUD_components/define_moud_nal.R">here</a>|
| MOUD (bup)   | Find the first occurrence of a bup prescription. <p>Defined <a href="https://github.com/CI-NYC/disability-chronic-pain/blob/93bbeb9d2edff361bf622a9889c7e1d811f0f238/scripts/06_define_OUD_components/define_moud_bup.R#L22">here</a> <p> best_list.rds is required and found at disability/projects/define_moud/input/best_list.rds | 

## Estimand 
We estimate the effect of each of the opioid exposure variables on each of the outcome variables, adjusting for covariates, holding other opioid exposure variables at their observed levels. Using the notation given above, this effect can be written: 
$E(Y_T^{d_n(\mathbf{A}), \Delta=1} - Y_T^{\mathbf{A}, \Delta=1}),$ where $E(Y_T^{\mathbf{A}, \Delta=1})$ denotes the expected value of the counterfactual outcome had the set of prescription opioid variables ($\mathbf{A}$) not been intervened on (i.e., remained as observed) and had no one been censored, and where $E(Y_T^{d_n(\mathbf{A}), \Delta=1})$ denotes the expected value of the counterfactual outcome had the particular opioid variable $A_n$ been intervened on as dictated by the function $d_n(\mathbf{A})$ but the remaining opioid variables stayed as observed and had no one been censored.

TO DO:
- look at the distributions of $A_1, A_2, A_3$ and decide what shift intervention makes sense.

