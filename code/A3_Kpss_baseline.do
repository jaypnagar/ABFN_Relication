*cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication

********************************************************************************

clear all
clear matrix
set more off
set maxvar 30000
*****************

*Analysis -
*Load Patent data
* recalculate the gamma and estimate the patent value

version 16
tempfile temp1 temp2 temp3



* Sample description
********************************************************************************
****************Joint***********************************************************
********************************************************************************
use  "Data\analyses.dta", clear


gen abnretsq = abnret * abnret
bysort permno_adj fyear: egen measured_vol = mean(abnretsq)

unique permno_adj
unique permno_adj if patent_firm //


*********kpss estimate

reghdfe ln_abnretsq_d02 patentaward if patent_firm==1  & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)

gen gamma_joint = _b[patentaward]
display gamma_joint

gen delta_joint = 1 - exp(-gamma_joint)
display delta_joint


/* estimate variance of measurement error */
bysort permno_adj fyear: gen tot_days = _N
bysort permno_adj fyear: egen pat_days = total(patentaward)
gen pat_days_ratio = pat_days / tot_days
gen vol_e_joint = 3 * measured_vol * ((1 + 3 * pat_days_ratio * (exp(gamma_joint) - 1))^-1)

sum vol_e_joint

gen sqrt_delta = sqrt(delta_joint)
gen functions_input = (-1) * (sqrt_delta) * (abnret_d02 / sqrt(vol_e_joint))
gen numerator = normalden(functions_input)
gen denominator = 1 - normal(functions_input)
gen exp_cond_value = delta_joint * abnret_d02 + sqrt_delta * sqrt(vol_e_joint) * (numerator / denominator)


gen pi = 0.56 // this is the unconditional probability of success
gen economic_value_kogan_fs = ((exp_cond_value * mktvalue) / (1 - pi)) / npatents

********************************************************************************
********************************************************************************
keep if patentaward == 1

replace economic_value_kogan_fs   = economic_value_kogan_fs / 1000000

gen patent_value_kpss = economic_value_kogan_fs

keep permno_adj date bdate npatents patentaward patent_value_kpss mktvalue
label var patent_value_kpss "Pat Val: KPSS (Baseline)"


save "Data\patent_value_kpss_baseline.dta", replace
