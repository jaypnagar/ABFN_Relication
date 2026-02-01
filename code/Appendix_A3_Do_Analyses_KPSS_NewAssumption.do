*cd "C:\Users\jayho\OneDrive - Duke University\KPSS_Replication\ABF_PatValue_Research"

/*********************************************************************************


***First part only create variables and key data****
*** all files used entity, code and year to merge
*
cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication"

gl OUT "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Output"

gl IN "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Data"


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


gen abnretsq = abnret * abnret
bysort permno_adj fyear: egen measured_vol = mean(abnretsq)

unique permno_adj
unique permno_adj if patent_firm //
unique permno_adj if patentscience_firm==1 & patentnoscience_firm==1 // 
unique permno_adj if patenttopq_cscience_firm==1 & patentnotopq_cscience_firm==1 // 
unique permno_adj if patentscience_adj_firm==1 & patentnoscience_adj_firm==1 // 


*******************KPSS Baseline Estimastion**************************************
**********************************************************************************

sum npatents if patentaward==1

/*

 Variable |        Obs        Mean    Std. dev.       Min        Max
-------------+---------------------------------------------------------
    npatents |    400,606    3.614614    8.561896          1        438

*/

bysort permno_adj fyear : gen tot_days = _N



gen pi = 0.56





********************************************************************************
********************************************************************************



reghdfe ln_abnretsq_d02 patentaward if patent_firm==1  & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)



gen gamma_joint = _b[patentaward]/3.61
display gamma_joint

gen delta_joint = 1-exp(-npatents*gamma_joint)
tab delta_joint



/* estimate variance of measurement error */

bysort permno_adj fyear npatents: egen pat_days = total(patentaward)
gen pat_days_ratio = pat_days / tot_days
gen vol_e_joint = 3 * measured_vol * ((1 + 3 * pat_days_ratio *npatents* (exp(gamma_joint) - 1))^-1)



gen sqrt_delta = sqrt(delta_joint)
tab sqrt_delta
gen functions_input = (-1) * (sqrt_delta) * (abnret_d02 / sqrt(vol_e_joint))
gen numerator = normalden(functions_input)
gen denominator = 1 - normal(functions_input)
gen exp_cond_value = delta_joint * abnret_d02 + sqrt_delta * sqrt(vol_e_joint) * (numerator / denominator)


*gen pi = 0.56 // this is the unconditional probability of success
gen economic_value_kogan_assum2 = ((exp_cond_value * mktvalue) / (1 - pi)) / npatents





keep if patentaward == 1

save "Data\patent_fullSample_assum2.dta", replace



replace economic_value_kogan_assum2   = economic_value_kogan_assum2 / 1000000



keep permno_adj date bdate economic_value_kogan_assum2 

sum economic_value_kogan_assum2 ,de

corr economic_value_kogan_assum2 

save "Data\patent_Value_topq_fullSample_assum2.dta", replace
