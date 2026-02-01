*cd "C:\Users\jayho\OneDrive - Duke University\KPSS_Replication\ABF_PatValue_Research"

********************************************************************************


/****First part only create variables and key data****
*** all files used entity, code and year to merge
*
cd "C:\Users\jayho\OneDrive - Duke University\ABF_PatValue_Research"

gl OUT "C:\Users\jayho\OneDrive - Duke University\ABF_PatValue_Research\Output"

gl IN "C:\Users\jayho\OneDrive - Duke University\ABF_PatValue_Research\Data"


******************************************************************************
Jay Office
*D:\jn206\Documents\OneDrive - Duke University\KPSS_Replication\ABF_PatValue_Research\Code

cd "D:\jn206\Documents\OneDrive - Duke University\ABF_PatValue_Research"

gl OUT "D:\jn206\Documents\OneDrive - Duke University\ABF_PatValue_Research\Output"

gl IN "D:\jn206\Documents\OneDrive - Duke University\ABF_PatValue_Research\Data"
*/


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




********************************************************************************
merge m:1 gvkey fyear using "Data\VertInteg_final_var.dta",keepusing(high_vertinteg topq_vertinteg vertinteg)

drop if _merge ==2
rename _merge merge_vertinteg

unique permno_adj if high_vertinteg!=.
tab fyear if high_vertinteg!=.

foreach var in high_vertinteg topq_vertinteg {
    
    gen pa_`var'_high = 0
    replace pa_`var'_high = 1 if patentaward == 1 & `var' == 1

    gen pa_`var'_low = 0
    replace pa_`var'_low = 1 if patentaward == 1 & `var' == 0
}
*****

keep if merge_vertinteg==3


unique permno_adj
unique permno_adj if patent_firm //



label var pa_high_vertinteg_high "pat: firm ver. Intg. (median): High"
label var pa_high_vertinteg_low "pat: firm ver. Intg. (median): low."

label var pa_topq_vertinteg_high "pat: firm ver. Intg. (topq): High"
label var pa_topq_vertinteg_low "pat: firm ver. Intg. (topq) low."

gen patentaward_vertinteg = patentaward * vertinteg
label var patentaward_vertinteg  "patentaward * vertinteg"
**************************************************
**************************************************
gen abnretsq = abnret * abnret
bysort permno_adj fyear: egen measured_vol = mean(abnretsq)

unique permno_adj
unique permno_adj if patent_firm //
unique permno_adj if patentscience_firm==1 & patentnoscience_firm==1 // 
unique permno_adj if patenttopq_cscience_firm==1 & patentnotopq_cscience_firm==1 // 
unique permno_adj if patentscience_adj_firm==1 & patentnoscience_adj_firm==1 // 


*******************KPSS Baseline Estimastion**************************************
**********************************************************************************

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
gen econ_value_kogan_verting = ((exp_cond_value * mktvalue) / (1 - pi)) / npatents


********************************************************************************
********************************************************************************

reghdfe ln_abnretsq_d02 pa_high_vertinteg_high pa_high_vertinteg_low if patent_firm==1  & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)



gen gamma_joint_high = _b[pa_high_vertinteg_high] 
display gamma_joint_high


gen delta_joint_high = 1 - exp(-gamma_joint_high)
display delta_joint_high


gen sqrt_delta_high = sqrt(delta_joint_high)
gen functions_input_high = (-1) * (sqrt_delta_high) * (abnret_d02 / sqrt(vol_e_joint))
gen numerator_high = normalden(functions_input_high)
gen denominator_high = 1 - normal(functions_input_high)
gen exp_cond_value_high = delta_joint_high * abnret_d02 + sqrt_delta_high * sqrt(vol_e_joint) * (numerator_high / denominator_high)


gen gamma_joint_low = _b[pa_high_vertinteg_low] 
display gamma_joint_low


gen delta_joint_low = 1 - exp(-gamma_joint_low)
display delta_joint_low


gen sqrt_delta_low = sqrt(delta_joint_low)
gen functions_input_low = (-1) * (sqrt_delta_low) * (abnret_d02 / sqrt(vol_e_joint))
gen numerator_low = normalden(functions_input_low)
gen denominator_low = 1 - normal(functions_input_low)
gen exp_cond_value_low = delta_joint_low * abnret_d02 + sqrt_delta_low * sqrt(vol_e_joint) * (numerator_low / denominator_low)


gen exp_cond_value_verting  = exp_cond_value_high if pa_high_vertinteg_high==1
replace exp_cond_value_verting  = exp_cond_value_low if pa_high_vertinteg_low==1


gen econ_value_kogan_verting_med = ((exp_cond_value_verting  * mktvalue) / (1 - pi)) / npatents


drop  gamma_joint_high  delta_joint_high sqrt_delta_high functions_input_high numerator_high denominator_high exp_cond_value_high gamma_joint_low gamma_joint_low delta_joint_low delta_joint_low sqrt_delta_low functions_input_low numerator_low denominator_low exp_cond_value_low exp_cond_value_verting
********************************************************************************
********************************************************************************

reghdfe ln_abnretsq_d02 pa_topq_vertinteg_high pa_topq_vertinteg_low if patent_firm==1  & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)



gen gamma_joint_high = _b[pa_topq_vertinteg_high] 
display gamma_joint_high


gen delta_joint_high = 1 - exp(-gamma_joint_high)
display delta_joint_high


gen sqrt_delta_high = sqrt(delta_joint_high)
gen functions_input_high = (-1) * (sqrt_delta_high) * (abnret_d02 / sqrt(vol_e_joint))
gen numerator_high = normalden(functions_input_high)
gen denominator_high = 1 - normal(functions_input_high)
gen exp_cond_value_high = delta_joint_high * abnret_d02 + sqrt_delta_high * sqrt(vol_e_joint) * (numerator_high / denominator_high)


gen gamma_joint_low = _b[pa_topq_vertinteg_low] 
display gamma_joint_low


gen delta_joint_low = 1 - exp(-gamma_joint_low)
display delta_joint_low


gen sqrt_delta_low = sqrt(delta_joint_low)
gen functions_input_low = (-1) * (sqrt_delta_low) * (abnret_d02 / sqrt(vol_e_joint))
gen numerator_low = normalden(functions_input_low)
gen denominator_low = 1 - normal(functions_input_low)
gen exp_cond_value_low = delta_joint_low * abnret_d02 + sqrt_delta_low * sqrt(vol_e_joint) * (numerator_low / denominator_low)


gen exp_cond_value_verting  = exp_cond_value_high if pa_high_vertinteg_high==1
replace exp_cond_value_verting  = exp_cond_value_low if pa_high_vertinteg_low==1


gen econ_value_kogan_verting_topq = ((exp_cond_value_verting  * mktvalue) / (1 - pi)) / npatents





drop  gamma_joint_high  delta_joint_high sqrt_delta_high functions_input_high numerator_high denominator_high exp_cond_value_high gamma_joint_low gamma_joint_low delta_joint_low delta_joint_low sqrt_delta_low functions_input_low numerator_low denominator_low exp_cond_value_low exp_cond_value_verting







keep if patentaward == 1




*************Convert in Mn***************************************

replace econ_value_kogan_verting = econ_value_kogan_verting / 1000000
replace econ_value_kogan_verting_med= econ_value_kogan_verting_med / 1000000
replace econ_value_kogan_verting_topq = econ_value_kogan_verting_topq/1000000




*winsor2 economic_value_kogan economic_value_science, cut(0 99) replace

sum  econ_value_kogan_verting econ_value_kogan_verting_med econ_value_kogan_verting_topq, d



gen patent_value_kpss_svi =econ_value_kogan_verting
gen patent_value_hignvi_med=econ_value_kogan_verting_med
gen patent_value_hignvi_topq = econ_value_kogan_verting_topq


label var  patent_value_kpss_svi "Pat Val: KPSS (baseline-VetIng sample)"
label var  patent_value_hignvi_med "Pat Val: High-Low int (Median)"
label var  patent_value_hignvi_topq "Pat Val: High-Low int (Topq)"


keep permno_adj date bdate pa_high_vertinteg_high pa_high_vertinteg_low pa_topq_vertinteg_high pa_topq_vertinteg_low patent_value_kpss_svi patent_value_hignvi_med patent_value_hignvi_topq



save "Data\patent_Value_econ_value_kogan_verting.dta", replace

