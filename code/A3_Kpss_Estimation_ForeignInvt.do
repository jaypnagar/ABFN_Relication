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

bysort permno_adj fyear: gen tot_days = _N
bysort permno_adj fyear: egen pat_days = total(patentaward)
gen pat_days_ratio = pat_days / tot_days

********************************************************************************
********************************************************************************

sum pat_allnonusa_invt_any pat_allnonusa_invt_any pat_allnonusa_invt_none
tab pat_allnonusa_invt_any pat_allnonusa_invt_none
tab pat_allnonusa_invt_any pat_allnonusa_invt_any

reghdfe ln_abnretsq_d02 pat_allnonusa_invt_any pat_allnonusa_invt_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)


test _b[pat_allnonusa_invt_none] =_b[pat_allnonusa_invt_any]


gen gamma_nononusa = _b[pat_allnonusa_invt_none]
display gamma_nononusa
gen delta_nononusa = 1 - exp(-gamma_nononusa)
display delta_nononusa

gen gamma_allnonusa = _b[pat_allnonusa_invt_any]
display gamma_allnonusa
gen delta_allnonusa = 1 - exp(-gamma_allnonusa)
display delta_allnonusa


bysort permno_adj fyear: egen pat_days_nononusa = total(pat_allnonusa_invt_none)
gen pat_days_ratio_nononusa = pat_days_nononusa / tot_days


bysort permno_adj fyear: egen pat_days_allonusa = total(pat_allnonusa_invt_any)
gen pat_days_ratio_allonusa= pat_days_allonusa / tot_days



gen vol_e_new = 3 * measured_vol * ((1 + (3 * pat_days_ratio_nononusa * (exp(gamma_nononusa) - 1)) + (3 * pat_days_ratio_allonusa * (exp(gamma_allnonusa) - 1)))^-1)


gen sqrt_delta_nononusa = sqrt(delta_nononusa)
gen functions_input_nononusa = (-1) * (sqrt_delta_nononusa) * (abnret_d02 / sqrt(vol_e_new))
gen numerator_nononusa = normalden(functions_input_nononusa)
gen denominator_nononusa = 1 - normal(functions_input_nononusa)
gen exp_cond_value_nononusa = delta_nononusa * abnret_d02 + sqrt_delta_nononusa * sqrt(vol_e_new) * (numerator_nononusa / denominator_nononusa)



gen sqrt_delta_allnonusa = sqrt(delta_allnonusa)
gen functions_input_allnonusa = (-1) * (sqrt_delta_allnonusa) * (abnret_d02 / sqrt(vol_e_new))
gen numerator_allnonusa = normalden(functions_input_allnonusa)
gen denominator_allnonusa = 1 - normal(functions_input_allnonusa)
gen exp_cond_value_allnonusa = delta_allnonusa * abnret_d02 + sqrt_delta_allnonusa * sqrt(vol_e_new) * (numerator_allnonusa / denominator_allnonusa)


gen pi = 0.56 // this is the unconditional probability of success
gen 	economic_value_allnonusa = ((exp_cond_value_nononusa * mktvalue) / (1 - pi)) / npatents if pat_allnonusa_invt_none == 1

replace economic_value_allnonusa = ((exp_cond_value_allnonusa * mktvalue) / (1 - pi)) / npatents if pat_allnonusa_invt_any == 1


********************************************************************************
********************************************************************************
keep if patentaward == 1


save "Data\patent_pythonallnonusa_invt.dta", replace



********************************************************************************
********************************************************************************

/* Run Python code for mixed type*/


// Run the Python script
shell "C:\Users\jayho\anaconda3\python.exe" "code\py_A3_Kpss_Estimation_ForeignInvt.py" 


********************************************************************************
********************************************************************************









preserve
clear all
use "Data\Multipatent_frompythonallnonusa_invt.dta" 

*gen original_date = 22487.00000000 // Example double format date
*gen date_string = string(date, "%td")

gen date_only = substr(date, 1, 10)
gen new_date = date(date_only, "YMD")
format new_date %td
drop date
rename new_date date
save "Data\Multipatent_frompythonallnonusa_invtv2.dta",replace
restore


merge 1:1 permno_adj date using "Data\Multipatent_frompythonallnonusa_invtv2.dta",keepusing(allnonusa_new nononusa_new pat_allnonusa_invt_mix)


keep permno_adj fyear bdate date  economic_value_allnonusa  pat_allnonusa_invt_any pat_allnonusa_invt_none npatents allnonusa_new nononusa_new pat_allnonusa_invt_mix




gen economic_value_allnonusa_only  = economic_value_allnonusa if pat_allnonusa_invt_mix==0 & pat_allnonusa_invt_any==1
replace economic_value_allnonusa_only  = allnonusa_new if pat_allnonusa_invt_mix==1 & pat_allnonusa_invt_any==1


gen economic_value_nononusa_only = economic_value_allnonusa if pat_allnonusa_invt_mix==0 & pat_allnonusa_invt_none==1
replace economic_value_nononusa_only  = nononusa_new if pat_allnonusa_invt_mix==1 & pat_allnonusa_invt_any==1


gen patent_foreign =pat_allnonusa_invt_any


replace economic_value_allnonusa_only = economic_value_allnonusa_only / 1000000
replace economic_value_nononusa_only = economic_value_nononusa_only / 1000000

replace economic_value_allnonusa = economic_value_allnonusa / 1000000



sort permno_adj date

gen patent_value_allnonusa_only =economic_value_allnonusa_only
gen patent_value_nononusa_only =economic_value_nononusa_only
gen patent_value_forinvt_noadj = economic_value_allnonusa

label var patent_value_allnonusa_only "Pat Val: All nonusa invt"
label var patent_value_nononusa_only "Pat Val: No nonusa invt"

label var patent_value_forinvt_noadj "Pat Val: All nonusa invt (no adj)"

keep permno_adj date bdate pat_allnonusa_invt_any pat_allnonusa_invt_none patent_value_allnonusa_only patent_value_nononusa_only economic_value_allnonusa  patent_foreign patent_value_forinvt_noadj

save "Data\patent_Value_allnonusa.dta", replace

