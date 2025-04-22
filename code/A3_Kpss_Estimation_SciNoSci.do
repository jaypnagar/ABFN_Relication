*cd "C:\Users\jayho\OneDrive - Duke University\KPSS_Replication\ABF_PatValue_Research"

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


*********top 3 quartile Full Sample

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


gen sqrt_delta = sqrt(delta_joint)
gen functions_input = (-1) * (sqrt_delta) * (abnret_d02 / sqrt(vol_e_joint))
gen numerator = normalden(functions_input)
gen denominator = 1 - normal(functions_input)
gen exp_cond_value = delta_joint * abnret_d02 + sqrt_delta * sqrt(vol_e_joint) * (numerator / denominator)


gen pi = 0.56 // this is the unconditional probability of success
gen economic_value_kogan_fs = ((exp_cond_value * mktvalue) / (1 - pi)) / npatents


********************************************************************************
********************************************************************************



********************************************************************************
********************************************************************************
reghdfe ln_abnretsq_d02 patents_topq_cscience_none patents_topq_cscience_any if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)


test _b[patents_topq_cscience_none] =_b[patents_topq_cscience_any]


*****************
gen gamma_noscience = _b[patents_topq_cscience_none]
display gamma_noscience
gen delta_noscience = 1 - exp(-gamma_noscience)
display delta_noscience

gen gamma_anyscience = _b[patents_topq_cscience_any]
display gamma_anyscience
gen delta_anyscience = 1 - exp(-gamma_anyscience)
display delta_anyscience

*****************

bysort permno_adj fyear: egen pat_days_nosci = total(patents_topq_cscience_none)
gen pat_days_ratio_nosci = pat_days_nosci / tot_days


bysort permno_adj fyear: egen pat_days_anysci = total(patents_topq_cscience_any)
gen pat_days_ratio_anysci = pat_days_anysci / tot_days



gen vol_e_new = 3 * measured_vol * ((1 + (3 * pat_days_ratio_nosci * (exp(gamma_noscience) - 1)) + (3 * pat_days_ratio_anysci * (exp(gamma_anyscience) - 1)))^-1)
*****************


gen sqrt_delta_noscience = sqrt(delta_noscience)
gen functions_input_noscience = (-1) * (sqrt_delta_noscience) * (abnret_d02 / sqrt(vol_e_new))
gen numerator_noscience = normalden(functions_input_noscience)
gen denominator_noscience = 1 - normal(functions_input_noscience)
gen exp_cond_value_noscience = delta_noscience * abnret_d02 + sqrt_delta_noscience * sqrt(vol_e_new) * (numerator_noscience / denominator_noscience)



gen sqrt_delta_anyscience = sqrt(delta_anyscience)
gen functions_input_anyscience = (-1) * (sqrt_delta_anyscience) * (abnret_d02 / sqrt(vol_e_new))
gen numerator_anyscience = normalden(functions_input_anyscience)
gen denominator_anyscience = 1 - normal(functions_input_anyscience)
gen exp_cond_value_anyscience = delta_anyscience * abnret_d02 + sqrt_delta_anyscience * sqrt(vol_e_new) * (numerator_anyscience / denominator_anyscience)
*****************


gen 	economic_value_science = ((exp_cond_value_noscience * mktvalue) / (1 - pi)) / npatents if patents_topq_cscience_none == 1

replace economic_value_science = ((exp_cond_value_anyscience * mktvalue) / (1 - pi)) / npatents if patents_topq_cscience_any == 1


********************************************************************************
********************************************************************************
keep if patentaward == 1





save "Data\patent_science_python_topq_fullSample.dta", replace

********************************************************************************
********************************************************************************

/* Run Python code for mixed type*/


// Run the Python script
shell "C:\Users\jayho\anaconda3\python.exe" "code\py_A3_Kpss_Estimation_SciNoSci.py" 


********************************************************************************
********************************************************************************
preserve
clear all
use "Data\Multipatent_fromPython_topq_fullSample.dta" 

*gen original_date = 22487.00000000 // Example double format date
*gen date_string = string(date, "%td")

gen date_only = substr(date, 1, 10)
gen new_date = date(date_only, "YMD")
format new_date %td
drop date
rename new_date date
save "Data\Multipatent_fromPython_topq_fullSample2_v2.dta",replace
restore


merge 1:1 permno_adj date using "Data\Multipatent_fromPython_topq_fullSample2_v2.dta",keepusing(science_new non_science_new patents_topq_cscience_mixed)


keep permno_adj fyear bdate date economic_value_kogan_fs economic_value_science  patents_topq_cscience_none patents_topq_cscience_any npatents science_new non_science_new patents_topq_cscience_mixed


gen economic_value_science_only = economic_value_science if patents_topq_cscience_mixed==0 & patents_topq_cscience_any==1

replace economic_value_science_only  = science_new if patents_topq_cscience_mixed==1 & patents_topq_cscience_any==1



gen economic_value_nonscience_only = economic_value_science if patents_topq_cscience_mixed==0 & patents_topq_cscience_none==1

replace economic_value_nonscience_only  = non_science_new if patents_topq_cscience_mixed==1 & patents_topq_cscience_any==1

gen patent_science =patents_topq_cscience_any



*************Convert in Mn***************************************
replace economic_value_kogan_fs   = economic_value_kogan_fs / 1000000
replace economic_value_science = economic_value_science/1000000
replace economic_value_science_only = economic_value_science_only / 1000000
replace economic_value_nonscience_only = economic_value_nonscience_only / 1000000





*winsor2 economic_value_kogan economic_value_science, cut(0 99) replace

sum economic_value_kogan_fs economic_value_science_only economic_value_nonscience_only, d







sort permno_adj date

gen patent_value_kogan_fs = economic_value_kogan_fs
gen patent_value_science_noadj = economic_value_science

gen patent_value_science_topq =economic_value_science_only
gen patent_value_nonscience_topq =economic_value_nonscience_only

keep permno_adj date bdate patents_topq_cscience_none patents_topq_cscience_mixed patents_topq_cscience_any npatents patent_value_kogan_fs patent_value_science_noadj patent_value_science_topq patent_value_nonscience_topq


label var patent_value_kogan_fs "Pat Val: KPSS (baseline)"
label var patent_value_science_noadj "Pat Val: SciNoSci (No Adj.)"
label var patent_value_science_topq "Pat Val: Sci (Adj.)"
label var patent_value_nonscience_topq "Pat Val: NoSci (Adj.)"

save "Data\patent_Value_topq_fullSample.dta", replace

