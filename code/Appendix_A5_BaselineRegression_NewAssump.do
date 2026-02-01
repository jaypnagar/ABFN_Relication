/* Set Directory */

/*Elia  cd "C:\Users\ef122\Duke University\Jay Prakash Nagar - ABF_PatValue_Research"*/
/*Jay */  cd "C:\Users\jayho\OneDrive - Duke University\ABF_PatValue_Research"


/*Jay  cd "D:\jn206\Documents\OneDrive - Duke University\ABF_PatValue_Research"  */


clear all


use "Data\Final_Patent_Level_Data.dta", clear

sum *

replace renewed_any =0 if renewed_any==.

label var trilateral_pat "Triadic patent"

drop if patent_value_science_kogan_cusd ==.
drop if economic_value_kogan_assum2_cusd ==.
drop if economic_assum2_newfor_cusd ==.


replace renewed_12th = . if year<1991 | year>2013

replace renewed_8th =. if  year<1991 | year>2017

replace renewed_any=. if  year<1991 | year>2019

corr patent_value_science_kogan_cusd economic_value_kogan_assum2_cusd economic_assum2_newfor_cusd renewed_any

corr patent_value_science_kogan_cusd economic_value_kogan_assum2_cusd economic_assum2_newfor_cusd renewed_any if npatents==1

corr patent_value_science_kogan_cusd economic_value_kogan_assum2_cusd economic_assum2_newfor_cusd renewed_any if npatents>1
*******************************************************
******************Summary Table: Science Non-Science****
********************************************************


est clear
eststo sumstats: qui estpost sum patent_value_science_kogan_cusd economic_value_kogan_assum2_cusd fwd_cits5 renewed_12th renewed_8th renewed_any renewal trilateral_pat family_size, d
esttab sumstats using "Output\SumTables\sumstats_newassump.tex", cells("count(fmt(0)) mean(fmt(3)) sd( fmt(3)) p10(fmt(3)) p50(fmt(3))  p90(fmt(3))") nonumber nomtitle noobs label collabels("Obs." "Mean" "Std. Dev." "10\%" "50\%" "90\%") replace


est clear
eststo sumstats: qui estpost sum patent_value_science_kogan_cusd economic_value_kogan_assum2_cusd fwd_cits5 renewed_12th renewed_8th renewed_any renewal trilateral_pat family_size  if npatents>1, d
esttab sumstats using "Output\SumTables\sumstats_newassump_multiple.tex", cells("count(fmt(0)) mean(fmt(3)) sd( fmt(3)) p10(fmt(3)) p50(fmt(3))  p90(fmt(3))") nonumber nomtitle noobs label collabels("Obs." "Mean" "Std. Dev." "10\%" "50\%" "90\%") replace

sum lpatvalue_kogan_fs lpatvalue_kogan_assum2 lpatvalue_assum2_newfor
********************************************************
******************Regression****************************
********************************************************



eststo clear
****************************************************************************************
xtset permno_adj 


***********************correlation table**********************************************
local vars lpatvalue_kogan_fs lpatvalue_kogan_assum2 fwd_cits5 renewed_12th renewed_8th renewed_any renewal trilateral_pat family_size
*Basic command:
corr `vars'
*More complicated
estpost correlate `vars',matrix 
esttab . using "Output\RegTables\Corr_tables_assump.tex", unstack b(3)  noobs nostar  nonumber replace label



***********************correlation table**********************************************
local vars lpatvalue_kogan_fs lpatvalue_kogan_assum2 fwd_cits5 renewed_12th renewed_8th renewed_any renewal trilateral_pat family_size if npatents>1
*Basic command:
corr `vars'
*More complicated
estpost correlate `vars',matrix 
esttab . using "Output\RegTables\Corr_tables_assump_multiple.tex", unstack b(3)  noobs nostar  nonumber replace label
********************************************************
******************Patent Quality**************************
********************************************************


est clear
* 1. OLS
* start with no controls, add quality controls

eststo: reghdfe log_fwd_cit5 , absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe log_fwd_cit5 , absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe log_fwd_cit5 lpatvalue_kogan_fs, absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe log_fwd_cit5 lpatvalue_kogan_fs, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe log_fwd_cit5 lpatvalue_kogan_assum2, absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe log_fwd_cit5 lpatvalue_kogan_assum2, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


esttab using "Output\RegTables\Table_V2_Assumption_FwdCites.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label("Avg of DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt( %9.3f %9.0gc  %9.0gc %9.0gc %9.4f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Only Fixed effect"  "KPSS Estimates" "New Assumption", pattern(1 0   1 0 1 0   ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
        

		
		
		
		
******************Full term renewal**************************
********************************************************


est clear
eststo: reghdfe renewed_12th if year>=1995 & year<=2012 , absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe renewed_12th if year>=1995 & year<=2012, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe renewed_12th lpatvalue_kogan_fs if year>=1995 & year<=2012, absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe renewed_12th lpatvalue_kogan_fs if year>=1995 & year<=2012, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe renewed_12th lpatvalue_kogan_assum2 if year>=1995 & year<=2012, absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe renewed_12th lpatvalue_kogan_assum2 if year>=1995 & year<=2012, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"




esttab using "Output\RegTables\Table_V2_Assumption_Renewalfullterm.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt( %9.3f %9.0gc  %9.0gc %9.0gc %9.4f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Only Fixed effect"  "KPSS Estimates" "New Assumption", pattern(1 0   1 0 1 0   ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
        
        

******************Full term renewal**************************
********************************************************


est clear
eststo: reghdfe renewed_8th if year>=1995 & year<=2017 , absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe renewed_8th if year>=1995 & year<=2017, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe renewed_8th lpatvalue_kogan_fs if year>=1995 & year<=2017, absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe renewed_8th lpatvalue_kogan_fs if year>=1995 & year<=2017, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe renewed_8th lpatvalue_kogan_assum2 if year>=1995 & year<=2017, absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe renewed_8th lpatvalue_kogan_assum2 if year>=1995 & year<=2017, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"



esttab using "Output\RegTables\Table_V2_Assumption_8th.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt(  %9.3f %9.0gc  %9.0gc %9.0gc %9.4f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Only Fixed effect"  "KPSS Estimates" "New Assumption", pattern(1 0   1 0 1 0   ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
        
		
******************Full term renewal**************************
********************************************************
est clear
eststo: reghdfe renewed_any if year>=1995 & year<=2019 , absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe renewed_any if year>=1995 & year<=2019, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe renewed_any lpatvalue_kogan_fs if year>=1995 & year<=2019, absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe renewed_any lpatvalue_kogan_fs if year>=1995 & year<=2019, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe renewed_any lpatvalue_kogan_assum2 if year>=1995 & year<=2019, absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe renewed_any lpatvalue_kogan_assum2 if year>=1995 & year<=2019, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"



esttab using "Output\RegTables\Table_V2_Assumption_any.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt(  %9.3f %9.0gc  %9.0gc %9.0gc %9.4f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Only Fixed effect"  "KPSS Estimates" "New Assumption", pattern(1 0   1 0 1 0   ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
        		

******************Full term renewal**************************
********************************************************				
				
est clear
eststo: reghdfe renewal if year>=1995 & year<=2017 , absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe renewal if year>=1995 & year<=2017, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe renewal lpatvalue_kogan_fs if year>=1995 & year<=2017, absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe renewal lpatvalue_kogan_fs if year>=1995 & year<=2017, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe renewal lpatvalue_kogan_assum2 if year>=1995 & year<=2017, absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe renewal lpatvalue_kogan_assum2 if year>=1995 & year<=2017, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"



esttab using "Output\RegTables\Table_V2_Assumption_renewal_inYear.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt(  %9.3f %9.0gc  %9.0gc %9.0gc %9.4f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Only Fixed effect"  "KPSS Estimates" "New Assumption", pattern(1 0   1 0 1 0   ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
        		
	  
********************************************************
******************trilateral_pat**************************
********************************************************	  


est clear
* 1. OLS
* start with no controls, add quality controls
est clear
eststo: reghdfe trilateral_pat, absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe trilateral_pat , absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe trilateral_pat lpatvalue_kogan_fs , absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe trilateral_pat lpatvalue_kogan_fs , absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe trilateral_pat lpatvalue_kogan_assum2 , absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe trilateral_pat lpatvalue_kogan_assum2 , absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"



esttab using "Output\RegTables\Table_V2_Assumption_trilateral_pat.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt(  %9.3f %9.0gc  %9.0gc %9.0gc %9.4f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Only Fixed effect"  "KPSS Estimates" "New Assumption", pattern(1 0   1 0 1 0   ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///

		
		********************************************************
******************trilateral_pat**************************
********************************************************	  


est clear
* 1. OLS
* start with no controls, add quality controls
est clear
eststo: reghdfe family_size, absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe family_size , absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe family_size lpatvalue_kogan_fs , absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe family_size lpatvalue_kogan_fs , absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe family_size lpatvalue_kogan_assum2 , absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe family_size lpatvalue_kogan_assum2 , absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"




esttab using "Output\RegTables\Table_V2_Assumption_log_family_size.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt(  %9.3f %9.0gc  %9.0gc %9.0gc %9.4f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Only Fixed effect"  "KPSS Estimates" "New Assumption", pattern(1 0   1 0 1 0   ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///

	   

********************************************************
******************Patent Value**************************
********************************************************


est clear
* 1. OLS
* start with no controls, add quality controls
eststo: reghdfe lpatvalue_kogan_fs topq_cscience, absorb(fiscal_year) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "No"
estadd local Firms = "No"


eststo: reghdfe lpatvalue_kogan_fs topq_cscience, absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"


eststo: reghdfe lpatvalue_kogan_fs topq_cscience, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpatvalue_kogan_fs topq_cscience log_fwd_cit5 claims, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpatvalue_kogan_assum2 topq_cscience, absorb(fiscal_year) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "No"
estadd local Firms = "No"

eststo: reghdfe lpatvalue_kogan_assum2 topq_cscience, absorb(fiscal_year ipc_4_digit) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe lpatvalue_kogan_assum2 topq_cscience, absorb(permno_adj fiscal_year ipc_4_digit) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpatvalue_kogan_assum2 topq_cscience log_fwd_cit5 claims, absorb(permno_adj fiscal_year ipc_4_digit) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"



esttab using "Output\RegTables\Table_V2_Assumption_Science.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label( "Avg DV"  "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.0gc %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("KPSS Patent Value Estimates" "New Assumption Estimates", pattern(1 0 0 0   1 0 0 0 0 ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(topq_cscience )


	   
	   
	   
	   
	   
	   
	   
	   
	   
********************************************************
******************Patent Value**************************
********************************************************


est clear
* 1. OLS
* start with no controls, add quality controls
eststo: reghdfe lpatvalue_kogan_fs topq_cscience, absorb(fiscal_year) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "No"
estadd local Firms = "No"


eststo: reghdfe lpatvalue_kogan_fs topq_cscience, absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"


eststo: reghdfe lpatvalue_kogan_fs topq_cscience, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpatvalue_kogan_fs topq_cscience log_fwd_cit5 claims, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpatvalue_scinosci_new topq_cscience, absorb(fiscal_year) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "No"
estadd local Firms = "No"

eststo: reghdfe lpatvalue_scinosci_new topq_cscience, absorb(fiscal_year ipc_4_digit) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe lpatvalue_scinosci_new topq_cscience, absorb(permno_adj fiscal_year ipc_4_digit) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpatvalue_scinosci_new topq_cscience log_fwd_cit5 claims, absorb(permno_adj fiscal_year ipc_4_digit) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"



esttab using "Output\RegTables\Table_V2_Assumption_SciNoSci_new.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label( "Avg DV"  "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.0gc %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("KPSS Patent Value Estimates" "New Assumption Estimates", pattern(1 0 0 0   1 0 0 0 0 ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(topq_cscience )
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
	   
gen new_npatents = npatents
replace new_npatents = 9999 if npatents>50
	
gen count_data = 1
	
	
	
	  
	  	  		  
collapse (mean) patent_value_science_kogan_cusd economic_value_kogan_assum2_cusd (sum) count_data,by(new_npatents)	  	  	  		  
	  