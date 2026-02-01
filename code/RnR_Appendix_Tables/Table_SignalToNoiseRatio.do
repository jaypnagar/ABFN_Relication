cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication"

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

*keep if merge_vertinteg==3


unique permno_adj
unique permno_adj if patent_firm //



label var pa_high_vertinteg_high "pat: firm ver. Intg. (median): High"
label var pa_high_vertinteg_low "pat: firm ver. Intg. (median): low."

label var pa_topq_vertinteg_high "pat: firm ver. Intg. (topq): High"
label var pa_topq_vertinteg_low "pat: firm ver. Intg. (topq) low."

gen patentaward_vertinteg = patentaward * vertinteg
label var patentaward_vertinteg  "patentaward * vertinteg"


gen abnretsq = abnret * abnret
bysort permno_adj fyear: egen measured_vol = mean(abnretsq)

unique permno_adj
unique permno_adj if patent_firm //





label var patentaward "Patent day"

label var patent_largeteam_topq_any "Patent day: Large Team Pat"
label var patent_largeteam_topq_none "Patent day: No Large Team Pat"

label var pat_allnonusa_invt_any "Patent day: Foreign Invt Pat"
label var pat_allnonusa_invt_none "Patent day: No Foreign Invt Pat"

label var patents_topq_cscience_any "Patent day: Science Pat"
label var patents_topq_cscience_none "Patent day: No Science Pat"


label var pa_topq_vertinteg_high "Patent day: High VI Firm "
label var pa_topq_vertinteg_low "Patent day: Low VI Firm"



**********check

eststo: reghdfe ln_abnretsq_d02 pat_75pernonusa_invt_any  pat_75pernonusa_invt_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"


eststo: reghdfe ln_abnretsq_d02 pat_67pernonusa_invt_any pat_67pernonusa_invt_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"











eststo clear
************Baseline Use in Main Analysis*************************************
est clear

eststo: reghdfe ln_abnretsq_d02 patentaward if patent_firm==1  & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo: reghdfe ln_abnretsq_d02 patent_largeteam_topq_any patent_largeteam_topq_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo: reghdfe ln_abnretsq_d02 pat_allnonusa_invt_any pat_allnonusa_invt_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"


eststo:reghdfe ln_abnretsq_d02 patents_topq_cscience_any patents_topq_cscience_none  if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo:reghdfe ln_abnretsq_d02 patentaward if patent_firm==1  & fyear<2020 &  merge_vertinteg==3, absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"


eststo:reghdfe ln_abnretsq_d02 pa_topq_vertinteg_high pa_topq_vertinteg_low if patent_firm==1 & merge_vertinteg==3 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)

qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"



esttab using "Output\snr_tables\Table_snr_baseline.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats(mean_dv firmyear date r2 N, ///
	label("Avg DV" "Firm*Year FE" "Date FE" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Baseline" "Team Size" "R\&D Offshoring" "Science" "Vertical Integration", pattern(1 1 1 1 1 0) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(patentaward )



	  
	  
	  
	  
	  
eststo clear
************Signal to Noise ration appendix*************************************	
est clear  
	  
eststo: reghdfe ln_abnretsq_d02 patentaward if patent_firm==1  & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"


	  
eststo:reghdfe ln_abnretsq_d02 patent_largeteam_any patent_largeteam_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"
	  

eststo: reghdfe ln_abnretsq_d02 pat_67pernonusa_invt_any pat_67pernonusa_invt_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"	  
	  
	  
eststo:reghdfe ln_abnretsq_d02 patents_cscience_adj_any patents_cscience_adj_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"  
	  
	  
eststo:reghdfe ln_abnretsq_d02 patentaward if patent_firm==1  & fyear<2020 &  merge_vertinteg==3, absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo: reghdfe ln_abnretsq_d02 pa_high_vertinteg_high pa_high_vertinteg_low if patent_firm==1 & merge_vertinteg==3  & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"
	  
	  
esttab using "Output\snr_tables\Table_snr_robustness.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats(mean_dv firmyear date r2 N, ///
	label("Avg DV" "Firm*Year FE" "Date FE" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Baseline" "Team Size" "R\&D Offshoring" "Science" "Vertical Integration", pattern(1 1 1 1 1 0) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(patentaward )

	  
	  
	  
/*	  
	  



est clear
* 1. OLS
* start with no controls, add quality controls

eststo: reghdfe ln_abnretsq_d02 patentaward if patent_firm==1  & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo:reghdfe ln_abnretsq_d02 patents_cscience_any patents_cscience_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo:reghdfe ln_abnretsq_d02 patents_topq_cscience_any patents_topq_cscience_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo:reghdfe ln_abnretsq_d02 patents_cscience_adj_any patents_cscience_adj_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo:reghdfe ln_abnretsq_d02 patents_top4q_cscience_any patents_top4q_cscience_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"


eststo:reghdfe ln_abnretsq_d02 patents_cscience_intext_any patents_cscience_intext_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"



esttab using "Output\snr_tables\Table1_ScienceCheck.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats(mean_dv firmyear date r2 N, ///
	label("Avg DV" "Firm*Year FE" "Date FE" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Science Patent Type", pattern(1 0  0  0 0 0 0) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(patentaward )

	  
	  

est clear
* 1. OLS	  
	  
eststo: reghdfe ln_abnretsq_d02 patentaward if patent_firm==1  & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo:reghdfe ln_abnretsq_d02 patent_largeteam_any patent_largeteam_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo:reghdfe ln_abnretsq_d02 patent_largeteam_topq_any patent_largeteam_topq_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo:reghdfe ln_abnretsq_d02 patent_multiteam_any patent_multiteam_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"



esttab using "Output\snr_tables\Table1_TeamCheck.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats(mean_dv firmyear date r2 N, ///
	label("Avg DV" "Firm*Year FE" "Date FE" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Science Patent Type", pattern(1 0  0  0 0 0 0) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(patentaward )	  
	  
	  

est clear
* 1. OLS	  
	  
eststo: reghdfe ln_abnretsq_d02 patentaward if patent_firm==1  & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo:reghdfe ln_abnretsq_d02 pat_allnonusa_invt_any pat_allnonusa_invt_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo:reghdfe ln_abnretsq_d02 pat_majornonusa_invt_any pat_majornonusa_invt_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"

eststo:reghdfe ln_abnretsq_d02 pat_atonenonus_invt_any pat_atonenonus_invt_none if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"


esttab using "Output\snr_tables\Table1_ForeignCheck.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats(mean_dv firmyear date r2 N, ///
	label("Avg DV" "Firm*Year FE" "Date FE" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Science Patent Type", pattern(1 0  0  0 0 0 0) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(patentaward )	  	  
	  
	  
	  
	  
	  
	  
	  
est clear
* 1. OLS	  
	  
eststo:reghdfe ln_abnretsq_d02 patentaward if patent_firm==1  & fyear<2020  & merge_vertinteg==3, absorb(permno_adj#fyear bdate) cluster(fyear)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"


eststo: reghdfe ln_abnretsq_d02 pa_topq_vertinteg_high pa_topq_vertinteg_low if patent_firm==1 & merge_vertinteg==3 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)

qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local firmyear = "Yes"
estadd local date = "Yes"


eststo: reghdfe ln_abnretsq_d02 pa_high_vertinteg_high pa_high_vertinteg_low if patent_firm==1 & merge_vertinteg==3  & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)



esttab using "Output\snr_tables\Table1_vi.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats(mean_dv firmyear date r2 N, ///
	label("Avg DV" "Firm*Year FE" "Date FE" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Science Patent Type", pattern(1 0  0  0 0 0 0) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(patentaward )	  	  	  