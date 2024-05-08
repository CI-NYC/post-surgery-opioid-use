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

| Exposure               | Definition |
|------------------------|------------|
| MME                    | MME dose conversion is defined <a href="https://github.com/CI-NYC/post_surgery_opioid_use/blob/main/R/02_exposures/02_05_dose_mme.R#L38">here</a>  <p>The mean daily dose of MME is calculated here using the function "calculate_mean_daily_dose |
| Days supplied          | Defined <a href="https://github.com/CI-NYC/post_surgery_opioid_use/blob/main/R/02_exposures/02_04_days_supplied.R#L25">here</a>
| Days of continuous use | Defined <a href="https://github.com/CI-NYC/post_surgery_opioid_use/blob/main/R/02_exposures/02_07_days_of_continuous_use.R">here</a>  
<p> Produces vectors for the lengths of all a beneficiary's periods of continuous use.                      |

| Outcome | Definition |
|---------|------------|
| OUD     | The dates of all OUD diagnosis codes for the cohort are identified <a href="https://github.com/CI-NYC/post_surgery_opioid_use/blob/main/R/00_get_vars/00_02_get_hillary.R">here</a>  <p> Whether the date of an OUD exists in the time period of interest is defined here <a href="https://github.com/CI-NYC/post_surgery_opioid_use/blob/main/R/03_outcomes/03_01_new_OD_and_OUD.R#L42">here</a>. Called "has_hillary"  <p> The earliest date of an eligible OUD diagnosis is defined here <a href = "https://github.com/CI-NYC/post_surgery_opioid_use/blob/main/R/03_outcomes/03_01_new_OD_and_OUD.R#L45-L53">here</a> |
| OD      | The dates of all OUD diagnosis codes for the cohort are identified  <a href="https://github.com/CI-NYC/post_surgery_opioid_use/blob/main/R/00_get_vars/00_01_get_poison.R">here</a>  <p> Whether the date of an OUD exists in the time period of interest is defined here <a href="https://github.com/CI-NYC/post_surgery_opioid_use/blob/main/R/03_outcomes/03_01_new_OD_and_OUD.R#L24">here</a>. Called "has_hillary"  <p> The earliest date of an eligible OUD diagnosis is defined here <a href = "https://github.com/CI-NYC/post_surgery_opioid_use/blob/main/R/03_outcomes/03_01_new_OD_and_OUD.R#L27-L35">here</a> |
| MOUD    | |

## Estimand 
We estimate the effect of each of the opioid exposure variables on each of the outcome variables, adjusting for covariates, holding other opioid exposure variables at their observed levels. Using the notation given above, this effect can be written: 
$E(Y_T^{d_n(\mathbf{A}), \Delta=1} - Y_T^{\mathbf{A}, \Delta=1}),$ where $E(Y_T^{\mathbf{A}, \Delta=1})$ denotes the expected value of the counterfactual outcome had the set of prescription opioid variables ($\mathbf{A}$) not been intervened on (i.e., remained as observed) and had no one been censored, and where $E(Y_T^{d_n(\mathbf{A}), \Delta=1})$ denotes the expected value of the counterfactual outcome had the particular opioid variable $A_n$ been intervened on as dictated by the function $d_n(\mathbf{A})$ but the remaining opioid variables stayed as observed and had no one been censored.

TO DO:
- look at the distributions of $A_1, A_2, A_3$ and decide what shift intervention makes sense.

