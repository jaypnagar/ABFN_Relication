*cd "C:\Users\jayho\OneDrive - Duke University\KPSS_Replication\ABF_PatValue_Research"

********************************************************************************

cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication"

gl OUT "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Output"

gl IN "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Data"

clear all


clear all


***Load data 
use "Data\Final_Patent_Level_Data_appendix.dta",clear

winsor2 pat_value_kpss_cusd pat_value_sci_median_sep_cusd pat_value_sci_median_mix_cusd  pat_value_teamsize_m_sep_cusd pat_value_teamsize_m_mix_cusd pat_value_23foreign_sep_cusd pat_value_23foreign_mix_cusd, cuts(1 99) replace









est clear
eststo sumstats: qui estpost sum  large_invt_team non_us_invt_67per cscience_adj  pa_high_vertinteg_high      pat_value_kpss_cusd pat_value_teamsize_m_mix_cusd  pat_value_23foreign_mix_cusd  pat_value_sci_median_mix_cusd  pat_value_kpss_cusd_svi pat_value_hignvi_medcusd,  d
esttab sumstats using "Output\AppendixTable_1_summarystats.tex", cells("count(fmt(0)) mean(fmt(3)) sd( fmt(3)) p10(fmt(3)) p50(fmt(3))  p90(fmt(3))") nonumber nomtitle noobs label collabels("Obs." "Mean" "Std. Dev." "10\%" "50\%" "90\%") replace







est clear
* 1. OLS
* start with no controls, add quality controls

eststo: reghdfe lpat_value_kpss large_invt_team lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe lpatvalue_teamsize_m_mix large_invt_team lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"



eststo: reghdfe lpat_value_kpss non_us_invt_67per lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe lpatvalue_23foreign_mix non_us_invt_67per lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe lpat_value_kpss cscience_adj lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe lpatvalue_sci_median_mix cscience_adj lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"




eststo: reghdfe lpat_value_kpss_cusd_svi pa_high_vertinteg_high  lmktvalue_cusd if merge_vi==3, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpat_value_hignvi_median pa_high_vertinteg_high lmktvalue_cusd if merge_vi==3, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"




esttab using "Output\AppendixTable_2_BaselineReg.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.0gc %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Teamsize" "R\&D Offshoring" "Science" "Vertical Integration" , pattern(1 0 1 0 1 0  1 0     ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(large_invt_team non_us_invt_67per cscience_adj pa_topq_vertinteg_high lmktvalue_cusd)
	  
	  
	  
