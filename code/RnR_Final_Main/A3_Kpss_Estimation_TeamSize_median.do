*cd "C:\Users\jayho\OneDrive - Duke University\KPSS_Replication\ABF_PatValue_Research"

********************************************************************************
cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication"



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


drop patent_largeteam_topq_any patent_largeteam_topq_none pat_largeteam_topq_mix


gen patent_largeteam_topq_any = patent_largeteam_any 
gen patent_largeteam_topq_none = patent_largeteam_none
 
gen pat_largeteam_topq_mix = pat_largeteam_mix








********************************************************************************
********************************************************************************


sum patent_largeteam_topq_any pat_largeteam_topq_mix patent_largeteam_topq_none

tab patent_largeteam_topq_any pat_largeteam_topq_mix
tab patentaward patent_largeteam_topq_any


tab patent_largeteam_topq_none pat_largeteam_topq_mix
 

reghdfe ln_abnretsq_d02 patent_largeteam_topq_any patent_largeteam_topq_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)


test _b[patent_largeteam_topq_any] =_b[patent_largeteam_topq_none]


gen gamma_smallteam = _b[patent_largeteam_topq_none]
display gamma_smallteam
gen delta_smallteam = 1 - exp(-gamma_smallteam)
display delta_smallteam

gen gamma_largeteam = _b[patent_largeteam_topq_any]
display gamma_largeteam
gen delta_largeteam = 1 - exp(-gamma_largeteam)
display delta_largeteam


bysort permno_adj fyear: egen pat_days_smallteam = total(patent_largeteam_topq_none)
gen pat_days_ratio_smallteam = pat_days_smallteam / tot_days


bysort permno_adj fyear: egen pat_days_largeteam = total(patent_largeteam_topq_any)
gen pat_days_ratio_largeteam= pat_days_largeteam / tot_days


gen vol_e_new = 3 * measured_vol * ((1 + (3 * pat_days_ratio_smallteam * (exp(gamma_smallteam) - 1)) + (3 * pat_days_ratio_largeteam * (exp(gamma_largeteam) - 1)))^-1)


gen sqrt_delta_smallteam = sqrt(delta_smallteam)
gen functions_input_smallteam = (-1) * (sqrt_delta_smallteam) * (abnret_d02 / sqrt(vol_e_new))
gen numerator_smallteam = normalden(functions_input_smallteam)
gen denominator_smallteam = 1 - normal(functions_input_smallteam)
gen exp_cond_value_smallteam = delta_smallteam * abnret_d02 + sqrt_delta_smallteam * sqrt(vol_e_new) * (numerator_smallteam / denominator_smallteam)



gen sqrt_delta_largeteam = sqrt(delta_largeteam)
gen functions_input_largeteam = (-1) * (sqrt_delta_largeteam) * (abnret_d02 / sqrt(vol_e_new))
gen numerator_largeteam = normalden(functions_input_largeteam)
gen denominator_largeteam = 1 - normal(functions_input_largeteam)
gen exp_cond_value_largeteam = delta_largeteam * abnret_d02 + sqrt_delta_largeteam * sqrt(vol_e_new) * (numerator_largeteam / denominator_largeteam)


gen pi = 0.56 // this is the unconditional probability of success
gen 	economic_value_largeteam = ((exp_cond_value_smallteam * mktvalue) / (1 - pi)) / npatents if patent_largeteam_topq_none == 1

replace economic_value_largeteam = ((exp_cond_value_largeteam * mktvalue) / (1 - pi)) / npatents if patent_largeteam_topq_any == 1


********************************************************************************
********************************************************************************
keep if patentaward == 1


save "Data\appendix\patent_pythonteamsize_topq_invt.dta", replace



********************************************************************************
********************************************************************************

/* Run Python code for mixed type*/


// Run the Python script
shell "C:\Users\jayho\anaconda3\python.exe" "code\RnR_1\RnR_Appendix_Tables\py_A3_Kpss_Estimation_TeamSize_appendix.py" 


********************************************************************************
********************************************************************************







preserve
clear all
use "Data\appendix\Multipatent_pythonteamsize_topq_invt.dta" 

*gen original_date = 22487.00000000 // Example double format date
*gen date_string = string(date, "%td")

gen date_only = substr(date, 1, 10)
gen new_date = date(date_only, "YMD")
format new_date %td
drop date
rename new_date date
save "Data\appendix\Multipatent_pythonteamsize_topq_invt_v2.dta",replace
restore


merge 1:1 permno_adj date using "Data\appendix\Multipatent_pythonteamsize_topq_invt_v2.dta",keepusing(largeteam_new smallteam_new pat_largeteam_topq_mix)


keep permno_adj fyear bdate date npatents economic_value_largeteam  patent_largeteam_topq_none patent_largeteam_topq_any largeteam_new smallteam_new pat_largeteam_topq_mix




gen economic_value_largeteam_only  = economic_value_largeteam if pat_largeteam_topq_mix==0 & patent_largeteam_topq_any==1
replace economic_value_largeteam_only  = largeteam_new if pat_largeteam_topq_mix==1 & patent_largeteam_topq_any==1


gen economic_value_smallteam_only = economic_value_largeteam if pat_largeteam_topq_mix==0 & patent_largeteam_topq_none==1
replace economic_value_smallteam_only  = smallteam_new if pat_largeteam_topq_mix==1 & patent_largeteam_topq_any==1


gen pat_largeteam_median =patent_largeteam_topq_any


replace economic_value_largeteam_only = economic_value_largeteam_only / 1000000
replace economic_value_smallteam_only = economic_value_smallteam_only / 1000000

replace economic_value_largeteam = economic_value_largeteam / 1000000



sort permno_adj date

gen patent_value_largeteam_m_only =economic_value_largeteam_only
gen patent_value_smallteam_m_only =economic_value_smallteam_only
gen patent_value_team_m_noadj = economic_value_largeteam

gen patent_largeteam_any =patent_largeteam_topq_any   
gen patent_largeteam_none = patent_largeteam_topq_none 
gen pat_largeteam_mix = pat_largeteam_topq_mix 








label var patent_value_largeteam_m_only "Pat Val: Large team invt(m)"
label var patent_value_smallteam_m_only "Pat Val: Small team invt(m)"

label var patent_value_team_m_noadj "Pat Val: team (no adj)(m)"

keep permno_adj date bdat patent_value_largeteam_m_only patent_value_smallteam_m_only patent_value_team_m_noadj patent_largeteam_any patent_largeteam_none pat_largeteam_mix

save "Data\appendix\patent_Value_teamsize_median.dta", replace


