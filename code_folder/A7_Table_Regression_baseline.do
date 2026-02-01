*cd "C:\Users\jayho\OneDrive - Duke University\KPSS_Replication\ABF_PatValue_Research"

********************************************************************************

cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication"

gl OUT "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Output"

gl IN "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Data"

clear all


clear all


***Load data 
use "Data\Final_Patent_Level_Data.dta",clear

winsor2 pat_value_kpss_cusd  pat_value_teamsize_topq_sep_cusd pat_value_foreign_mix_cusd pat_value_foreign_sep_cusd pat_value_teamsize_topq_mix_cusd pat_value_sci_topq_sep_cusd pat_value_sci_topq_mix_cusd    pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd, cuts(1 99) replace



label var topq_cscience "\$\mathds{1}\$[Patent Science Dummy]"
label var all_nonusa_invt "\$\mathds{1}\$[All Foreign Inventor]"
label var large_invt_team_topq "\$\mathds{1}\$[Team Size: Large]"
label var pa_topq_vertinteg_high "\$\mathds{1}\$[Vertical Integration: High]"
label var breakthrough "\$\mathds{1}\$[Top 1\% of Cited Patents]"
label var renewed_12th "\$\mathds{1}\$[Renewed (Full Term)]"
label var reassign_dummy "\$\mathds{1}\$[Reassignment]"
label var trilateral_pat "\$\mathds{1}\$[Trilateral Patent]"
label var litigation "\$\mathds{1}\$[Litigation]"


 
 
 
label var pat_value_kpss_cusd "Pat Val: KPSS (const. usd)"
label var pat_value_teamsize_topq_sep_cusd "Pat Val: Teamsize (sep)"
label var pat_value_teamsize_topq_mix_cusd   "Pat Val: Teamsize (mixed)"


label var pat_value_foreign_sep_cusd "Pat Val: Foreign (sep)"
label var pat_value_foreign_mix_cusd "Pat Val: Foreign (mixed)"
 
label var pat_value_sci_topq_sep_cusd "Pat Val: Sci\&NoSci (sep)"
label var pat_value_sci_topq_mix_cusd "Pat Val: Sci\&NoSci (mixed)"

label var pat_value_kpss_cusd_svi "Pat Val: KPSS (VI)"
label var pat_value_hignvi_topqcusd "Pat Val: Vertical Integration (sep)"



sum pat_value_kpss_cusd pat_value_sci_topq_mix_cusd pat_value_sci_topq_sep_cusd pat_value_foreign_mix_cusd pat_value_foreign_mix_cusd pat_value_teamsize_topq_mix_cusd pat_value_teamsize_topq_sep_cusd pat_value_kpss_cusd_svi


/*est clear
eststo sumstats: qui estpost sum pat_value_kpss_cusd pat_value_sci_topq_mix_cusd pat_value_sci_topq_sep_cusd pat_value_foreign_mix_cusd pat_value_foreign_sep_cusd pat_value_teamsize_topq_mix_cusd pat_value_teamsize_topq_sep_cusd topq_cscience all_nonusa_invt large_invt_team_topq, d
esttab sumstats using "Output\table_1_summarystats.tex", cells("count(fmt(0)) mean(fmt(3)) sd( fmt(3)) p10(fmt(3)) p50(fmt(3))  p90(fmt(3))") nonumber nomtitle noobs label collabels("Obs." "Mean" "Std. Dev." "10\%" "50\%" "90\%") replace


********************************************************************************
**********Merging Patent value for science and Non-science patent***************
********************************************************************************
gen topq_other = 0 
replace topq_other =1 if topq_cscience==0
eststo clear
eststo topq_cscience: quietly estpost summarize pat_value_kpss_cusd pat_value_sci_topq_mix_cusd pat_value_sci_topq_sep_cusd if topq_cscience==1
eststo topq_other: quietly  estpost summarize pat_value_kpss_cusd pat_value_sci_topq_mix_cusd pat_value_sci_topq_sep_cusd if topq_cscience==0

eststo diff: quietly estpost ttest pat_value_kpss_cusd pat_value_sci_topq_mix_cusd pat_value_sci_topq_sep_cusd, by(topq_other)

esttab topq_cscience topq_other diff using "Output\table_2_panel_a.tex", replace cells("mean(pattern(1 1 0) fmt(3)) sd(pattern(1 1 0)) b(star pattern(0 0 1) fmt(3))") label order()

*/


sum lpat_value_kpss lpatvalue_sci_topq_mix lpatvalue_sci_sep_topq


eststo clear
****************************************************************************************
xtset permno_adj 









est clear
* 1. OLS
* start with no controls, add quality controls
eststo: reghdfe lpat_value_kpss topq_cscience, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe lpat_value_kpss topq_cscience lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpatvalue_sci_sep_topq topq_cscience, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe lpatvalue_sci_sep_topq topq_cscience lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpatvalue_sci_topq_mix topq_cscience, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe lpatvalue_sci_topq_mix topq_cscience lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"



esttab using "Output\Table_3_PatValueScience.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.0gc %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("KPSS Estimates" "Science and Non-Science Patent day" "Modified estimates on Mixed patent day ", pattern(1 0    1 0  1 0   ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(topq_cscience )

















********************************************************************************
**********Foreing Domestic: All Non-USA inventors ***************
********************************************************************************
gen all_nonusa_invt_other = 0 
replace all_nonusa_invt_other =1 if all_nonusa_invt==0

/*eststo clear
eststo all_nonusa_invt: quietly estpost summarize pat_value_kpss_cusd pat_value_foreign_mix_cusd pat_value_foreign_sep_cusd if all_nonusa_invt==1
eststo all_nonusa_invt_other: quietly  estpost summarize pat_value_kpss_cusd pat_value_foreign_mix_cusd pat_value_foreign_sep_cusd if all_nonusa_invt==0

eststo diff: quietly estpost ttest pat_value_kpss_cusd pat_value_foreign_mix_cusd pat_value_foreign_sep_cusd, by(all_nonusa_invt_other)

esttab all_nonusa_invt all_nonusa_invt_other diff using "Output\table_2_panel_b.tex", replace cells("mean(pattern(1 1 0) fmt(3)) sd(pattern(1 1 0)) b(star pattern(0 0 1) fmt(3))") label order()




sum lpat_value_kpss lpatvalue_foreign_mix lpatvalue_foreign_sep_topq

*/


eststo clear
****************************************************************************************
xtset permno_adj 






est clear
* 1. OLS
* start with no controls, add quality controls
eststo: reghdfe lpat_value_kpss all_nonusa_invt, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe lpat_value_kpss all_nonusa_invt lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpatvalue_foreign_sep_topq all_nonusa_invt, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe lpatvalue_foreign_sep_topq all_nonusa_invt lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpatvalue_foreign_mix all_nonusa_invt, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe lpatvalue_foreign_mix all_nonusa_invt lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"



esttab using "Output\Table_4_ForeinDomestic_inventor.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.0gc %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("KPSS Estimates" "Forein and Domestic Patent day" "Modified estimates on Mixed patent day ", pattern(1 0    1 0  1 0   ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(all_nonusa_invt )











********************************************************************************
**********Inventor Team Size (based on topq) ***************
*******************************************************************************
gen large_invt_team_other = 0 
replace large_invt_team_other =1 if large_invt_team_topq==0
/*
eststo clear
eststo large_invt_team_topq: quietly estpost summarize pat_value_kpss_cusd pat_value_teamsize_topq_mix_cusd pat_value_teamsize_topq_sep_cusd if large_invt_team_topq==1
eststo large_invt_team_other: quietly  estpost summarize pat_value_kpss_cusd pat_value_teamsize_topq_mix_cusd pat_value_teamsize_topq_sep_cusd if large_invt_team_topq==0

eststo diff: quietly estpost ttest pat_value_kpss_cusd pat_value_teamsize_topq_mix_cusd pat_value_teamsize_topq_sep_cusd, by(large_invt_team_other)

esttab large_invt_team_topq large_invt_team_other diff using "Output\table_2_panel_c.tex", replace cells("mean(pattern(1 1 0) fmt(3)) sd(pattern(1 1 0)) b(star pattern(0 0 1) fmt(3))") label order()

*/


sum lpat_value_kpss lpatvalue_teamsize_topq_mix lpatvalue_teamsize_sep_topq




eststo clear
****************************************************************************************
xtset permno_adj 






est clear
* 1. OLS
* start with no controls, add quality controls
eststo: reghdfe lpat_value_kpss large_invt_team_topq, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe lpat_value_kpss large_invt_team_topq lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpatvalue_teamsize_sep_topq large_invt_team_topq, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe lpatvalue_teamsize_sep_topq large_invt_team_topq lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpatvalue_teamsize_topq_mix large_invt_team_topq, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe lpatvalue_teamsize_topq_mix large_invt_team_topq lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"



esttab using "Output\Table_4_TeamSize.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.0gc %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("KPSS Estimates" "Large and Small team Patent day" "Modified estimates on Mixed patent day ", pattern(1 0    1 0  1 0   ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(large_invt_team_topq )







********************************************************************************
**********Vertical Integration ***************
******************************************************************************
keep if  merge_vi==3

/**
est clear
eststo sumstats: qui estpost sum pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd pa_topq_vertinteg_high, d
esttab sumstats using "Output\table_1_panel_b.tex", cells("count(fmt(0)) mean(fmt(3)) sd( fmt(3)) p10(fmt(3)) p50(fmt(3))  p90(fmt(3))") nonumber nomtitle noobs label collabels("Obs." "Mean" "Std. Dev." "10\%" "50\%" "90\%") replace


eststo clear
eststo pa_topq_vertinteg_high: quietly estpost summarize pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd  if pa_topq_vertinteg_high==1
eststo pa_topq_vertinteg_low: quietly  estpost summarize pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd if pa_topq_vertinteg_high==0

eststo diff: quietly estpost ttest pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd, by(pa_topq_vertinteg_high)

esttab pa_topq_vertinteg_high pa_topq_vertinteg_low diff using "Output\table_2_panel_d.tex", replace cells("mean(pattern(1 1 0) fmt(3)) sd(pattern(1 1 0)) b(star pattern(0 0 1) fmt(3))") label order()
*/





********************************************************************************
**********Vertical Integration ***************
********************************************************************************
keep if  merge_vi==3


est clear
eststo sumstats: qui estpost sum pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd pa_topq_vertinteg_high, d
esttab sumstats using "Output\table_1_panel_b.tex", cells("count(fmt(0)) mean(fmt(3)) sd( fmt(3)) p10(fmt(3)) p50(fmt(3))  p90(fmt(3))") nonumber nomtitle noobs label collabels("Obs." "Mean" "Std. Dev." "10\%" "50\%" "90\%") replace


eststo clear
eststo pa_topq_vertinteg_high: quietly estpost summarize pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd  if pa_topq_vertinteg_high==1
eststo pa_topq_vertinteg_low: quietly  estpost summarize pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd if pa_topq_vertinteg_high==0

eststo diff: quietly estpost ttest pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd, by(pa_topq_vertinteg_high)

esttab pa_topq_vertinteg_high pa_topq_vertinteg_low diff using "Output\table_2_panel_d.tex", replace cells("mean(pattern(1 1 0) fmt(3)) sd(pattern(1 1 0)) b(star pattern(0 0 1) fmt(3))") label order()





est clear
* 1. OLS
* start with no controls, add quality controls
eststo: reghdfe lpat_value_kpss_cusd_svi pa_topq_vertinteg_high, absorb(fiscal_year ipc_4_digit ) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe lpat_value_kpss_cusd_svi pa_topq_vertinteg_high lmktvalue_cusd, absorb(fiscal_year ipc_4_digit ) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe lpat_value_kpss_cusd_svi pa_topq_vertinteg_high  lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe lpat_value_hignvi_topq pa_topq_vertinteg_high, absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe lpat_value_hignvi_topq pa_topq_vertinteg_high lmktvalue_cusd, absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe lpat_value_hignvi_median pa_topq_vertinteg_high lmktvalue_cusd, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"




esttab using "Output\Table_5_VeticalIntegration.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.0gc %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("KPSS Estimates" "Vetical Intgration " , pattern(1 0 0   1 0 0    ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(pa_topq_vertinteg_high )


********************************************************************************
******************************Appendix Table************************************
********************************************************************************
clear all
***Load data 
use "Data\Final_Patent_Level_Data.dta",clear


replace breakthrough = breakthrough*100

est clear
* 1. OLS
* start with no controls, add quality controls

eststo: reghdfe log_fwd_cit5 , absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe log_fwd_cit5 lpat_value_kpss, absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe log_fwd_cit5 lpatvalue_kogan_assum2, absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe log_fwd_cit5, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe log_fwd_cit5 lpat_value_kpss, absorb(fiscal_year ipc_4_digit permno_adj ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe log_fwd_cit5 lpatvalue_kogan_assum2, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"




eststo: reghdfe breakthrough , absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe breakthrough lpat_value_kpss, absorb(fiscal_year ipc_4_digit) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe breakthrough lpatvalue_kogan_assum2, absorb(fiscal_year ipc_4_digit ) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "No"

eststo: reghdfe breakthrough, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


eststo: reghdfe breakthrough lpat_value_kpss, absorb(fiscal_year ipc_4_digit  permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"

eststo: reghdfe breakthrough lpatvalue_kogan_assum2, absorb(fiscal_year ipc_4_digit permno_adj) vce(robust)  
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs = "Yes"
estadd local Firms = "Yes"


esttab using "Output\AppendixTable_1_Assumption_FwdCites.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats(mean_dv Years IPCs Firms r2 N, ///
	label("Avg of DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "R^2" "N") ///
	 fmt( %9.3f %9.0gc  %9.0gc %9.0gc %9.4f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Log(1+Fwd citations)"  "Breakthrough", pattern(1 0 0 0 0 0 0 1 0 0 0 0 0 0 0  ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
        

preserve		
gen new_npatents = npatents
replace new_npatents = 9999 if npatents>30
	
gen count_data = 1

egen mean_kpss =mean(pat_value_kpss_cusd)
egen mean_kpss_newassump =mean(economic_value_kogan_assum2_cusd)
  	  		  
collapse (mean) pat_value_kpss_cusd (mean) economic_value_kogan_assum2_cusd   (mean) mean_kpss (mean) mean_kpss_newassump          (sum) count_data,by(new_npatents)	  		
		
export excel using "Output\AppendixChart_KPSS.xlsx", firstrow(variables) replace		
restore		