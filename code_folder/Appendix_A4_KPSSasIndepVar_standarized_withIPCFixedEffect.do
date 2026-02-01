*cd "C:\Users\jayho\OneDrive - Duke University\KPSS_Replication\ABF_PatValue_Research"

********************************************************************************

cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication"

gl OUT "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Output"

gl IN "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Data"

clear all



/* Part 1: Correlation between patent value estimates and future patent outcomes */

* Load data 
use "Data\Final_Patent_Level_Data.dta", clear

merge m:1 permno_adj date using "Data\analyses.dta", keepusing(abnret_d02 npatents)
drop if _merge == 2
drop _merge

* Other variables
gen abnret_d02_adj = abnret_d02 / npatents
egen ipc = group(ipc_4_digit)
replace breakthrough = breakthrough*100

replace litigation = litigation*100

label var trilateral_pat "I[Trilateral Patent]"
label var litigation "I[Litigation]x100"


replace renewed_12th=. if fiscal_year<1992 |fiscal_year>2012

winsor2 abnret_d02 pat_value_kpss_cusd pat_value_sci_topq_mix_cusd pat_value_teamsize_topq_mix_cusd pat_value_foreign_mix_cusd pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd, cuts(1 99) replace


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




****************************************************************************************
****************************************************************************************



gen return_cusd = mktvalue_cusd*abnret_d02
label var return_cusd "Abnormal Return*Market return"



sum mktvalue_cusd

***standarized the values

sum return_cusd pat_value_kpss_cusd pat_value_teamsize_topq_mix_cusd pat_value_foreign_mix_cusd pat_value_sci_topq_mix_cusd pat_value_sci_topq_mix_cusd pat_value_hignvi_topqcusd


foreach v in ///
    return_cusd ///
    pat_value_kpss_cusd ///
    pat_value_teamsize_topq_mix_cusd ///
    pat_value_foreign_mix_cusd ///
    pat_value_sci_topq_mix_cusd ///
	pat_value_kpss_cusd_svi ///
    pat_value_hignvi_topqcusd {

    local base = subinstr("`v'", "_cusd", "", .)
    egen z_`base' = std(`v')
}


label var z_return           "Abnormal Return × Market Value (std.)"
label var z_pat_value_kpss   "Pat Val: KPSS (std.)"
label var z_pat_value_teamsize_topq_mix ///
                             "Pat Val: Teamsize (mixed, std.)"
label var z_pat_value_foreign_mix ///
                             "Pat Val: Foreign (mixed, std.)"
label var z_pat_value_sci_topq_mix ///
                             "Pat Val: Sci & NoSci (mixed, std.)"
label var z_pat_value_kpss_svi ///
                             "Pat Val: KPSS (VI, std.)"
label var z_pat_value_hignvi_topq ///
                             "Pat Val: Vertical Integration (std.)"


							 
							 
eststo clear
****************************************************************************************
****************************************************************************************
xtset permno_adj 


********************************************************************************
* ASSUMPTION: you created standardized vars WITHOUT "_cusd" suffix, like:
*   z_return
*   z_pat_value_kpss
*   z_pat_value_teamsize_topq_mix
*   z_pat_value_foreign_mix
*   z_pat_value_sci_topq_mix
*   z_pat_value_kpss_svi          (if you standardized pat_value_kpss_cusd_svi)
*   z_pat_value_hignvi_topq       (from pat_value_hignvi_topqcusd)
*
* NOTE: return variable is return_cusd (not sum_return_cusd)
********************************************************************************

*******************************************************
* Forward cites table: replace SE in parentheses with se_boot
* (Only for the tested regressor in each column)
*******************************************************

**************************************
* Forward citations (fwd_cits5)
**************************************
est clear

eststo: reghdfe fwd_cits5 z_return lmktvalue_cusd i.fiscal_year, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"



eststo: reghdfe fwd_cits5 z_pat_value_kpss lmktvalue_cusd i.fiscal_year, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe fwd_cits5 z_pat_value_teamsize_topq_mix lmktvalue_cusd i.fiscal_year, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe fwd_cits5 z_pat_value_foreign_mix lmktvalue_cusd i.fiscal_year, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"



eststo: reghdfe fwd_cits5 z_pat_value_sci_topq_mix lmktvalue_cusd i.fiscal_year, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

* VI sample
eststo: reghdfe fwd_cits5 z_pat_value_kpss_svi lmktvalue_cusd i.fiscal_year if merge_vi==3, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe fwd_cits5 z_pat_value_hignvi_topq lmktvalue_cusd i.fiscal_year if merge_vi==3, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"


esttab using "Output\Table_1_v2_Fwd_all_std_withipc.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	drop(*.fiscal_year ) ///
    stats(mean_dv Years IPCs Firms ll N, ///  
    label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "Log Likelihood" "N") ///
    fmt(%9.3f %9.0gc %9.0gc %9.0gc %9.3f %12.0gc)) ///
    starlevels(* .10 ** .05 *** .01) label collabels(none) ///
    nomtitle ///
    mgroups("Forward Citation:All" "Forward Citation:VI Sample", pattern(1 0 0 0 0 1 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) ///
        span erepeat(\cmidrule(lr){@span})) ///
    order(z_return z_pat_value_kpss z_pat_value_teamsize_topq_mix z_pat_value_foreign_mix z_pat_value_sci_topq_mix z_pat_value_kpss_svi z_pat_value_hignvi_topq lmktvalue_cusd)



**************************************
* Breakthrough
**************************************
est clear

eststo: reghdfe breakthrough z_return lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe breakthrough z_pat_value_kpss lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe breakthrough z_pat_value_teamsize_topq_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe breakthrough z_pat_value_foreign_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe breakthrough z_pat_value_sci_topq_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

* VI sample
eststo: reghdfe breakthrough z_pat_value_kpss_svi lmktvalue_cusd i.fiscal_year  if merge_vi==3, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe breakthrough z_pat_value_hignvi_topq lmktvalue_cusd i.fiscal_year  if merge_vi==3, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"


esttab using "Output\Table_1_v2_breakthrough_all_std_withipc.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	drop(*.fiscal_year ) ///
    stats(mean_dv Years IPCs Firms ll N, ///
    label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "Log Likelihood" "N") ///
    fmt(%9.3f %9.0gc %9.0gc %9.0gc %9.3f %12.0gc)) ///
    starlevels(* .10 ** .05 *** .01) label collabels(none) ///
    nomtitle ///
    mgroups("Breakthrough:All" "Breakthrough: VI Sample", pattern(1 0 0 0 0 1 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) ///
        span erepeat(\cmidrule(lr){@span})) ///
    order(z_return z_pat_value_kpss z_pat_value_teamsize_topq_mix z_pat_value_foreign_mix z_pat_value_sci_topq_mix z_pat_value_kpss_svi z_pat_value_hignvi_topq lmktvalue_cusd  )



**************************************
* Renewed full term (renewed_12th)
**************************************
est clear

eststo: reghdfe renewed_12th z_return lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe renewed_12th z_pat_value_kpss lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe renewed_12th z_pat_value_teamsize_topq_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe renewed_12th z_pat_value_foreign_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe renewed_12th z_pat_value_sci_topq_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

* VI sample
eststo: reghdfe renewed_12th z_pat_value_kpss_svi lmktvalue_cusd i.fiscal_year  if merge_vi==3, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe renewed_12th z_pat_value_hignvi_topq lmktvalue_cusd i.fiscal_year  if merge_vi==3, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"


esttab using "Output\Table_1_v2_renewed_12th_all_std_withipc.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	drop(*.fiscal_year ) ///
    stats(mean_dv Years IPCs Firms ll N, ///
    label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "Log Likelihood" "N") ///
    fmt(%9.3f %9.0gc %9.0gc %9.0gc %9.3f %12.0gc)) ///
    starlevels(* .10 ** .05 *** .01) label collabels(none) ///
    nomtitle ///
    mgroups("Renewed Full Term:All" "Renewed Full Term: VI Sample", pattern(1 0 0 0 0 1 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) ///
        span erepeat(\cmidrule(lr){@span})) ///
    order(z_return z_pat_value_kpss z_pat_value_teamsize_topq_mix z_pat_value_foreign_mix z_pat_value_sci_topq_mix z_pat_value_kpss_svi z_pat_value_hignvi_topq lmktvalue_cusd  )



**************************************
* Reassign dummy
**************************************
est clear

eststo: reghdfe reassign_dummy z_return lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe reassign_dummy z_pat_value_kpss lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe reassign_dummy z_pat_value_teamsize_topq_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe reassign_dummy z_pat_value_foreign_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe reassign_dummy z_pat_value_sci_topq_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

* VI sample
eststo: reghdfe reassign_dummy z_pat_value_kpss_svi lmktvalue_cusd i.fiscal_year  if merge_vi==3, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe reassign_dummy z_pat_value_hignvi_topq lmktvalue_cusd i.fiscal_year  if merge_vi==3, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"


esttab using "Output\Table_1_v2_reassign_dummy_all_std_withipc.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	drop(*.fiscal_year ) ///
    stats(mean_dv Years IPCs Firms ll N, ///
    label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "Log Likelihood" "N") ///
    fmt(%9.3f %9.0gc %9.0gc %9.0gc %9.3f %12.0gc)) ///
    starlevels(* .10 ** .05 *** .01) label collabels(none) ///
    nomtitle ///
    mgroups("Reassign: All" "Reassign: VI Sample", pattern(1 0 0 0 0 1 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) ///
        span erepeat(\cmidrule(lr){@span})) ///
    order(z_return z_pat_value_kpss z_pat_value_teamsize_topq_mix z_pat_value_foreign_mix z_pat_value_sci_topq_mix z_pat_value_kpss_svi z_pat_value_hignvi_topq lmktvalue_cusd )



**************************************
* Trilateral patent
**************************************
est clear

eststo: reghdfe trilateral_pat z_return lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe trilateral_pat z_pat_value_kpss lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe trilateral_pat z_pat_value_teamsize_topq_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe trilateral_pat z_pat_value_foreign_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe trilateral_pat z_pat_value_sci_topq_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

* VI sample
eststo: reghdfe trilateral_pat z_pat_value_kpss_svi lmktvalue_cusd i.fiscal_year  if merge_vi==3, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe trilateral_pat z_pat_value_hignvi_topq lmktvalue_cusd i.fiscal_year  if merge_vi==3, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"


esttab using "Output\Table_1_v2_trilateral_pat_all_std_withipc.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
    drop(*.fiscal_year ) ///
	stats(mean_dv Years IPCs Firms ll N, ///
    label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "Log Likelihood" "N") ///
    fmt(%9.3f %9.0gc %9.0gc %9.0gc %9.3f %12.0gc)) ///
    starlevels(* .10 ** .05 *** .01) label collabels(none) ///
    nomtitle ///
    mgroups("Trilateral Patent:All" "Trilateral Patent:VI", pattern(1 0 0 0 0 1 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) ///
        span erepeat(\cmidrule(lr){@span})) ///
    order(z_return z_pat_value_kpss z_pat_value_teamsize_topq_mix z_pat_value_foreign_mix z_pat_value_sci_topq_mix z_pat_value_kpss_svi z_pat_value_hignvi_topq lmktvalue_cusd  )



**************************************
* Litigation
**************************************
est clear

eststo: reghdfe litigation z_return lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe litigation z_pat_value_kpss lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe litigation z_pat_value_teamsize_topq_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe litigation z_pat_value_foreign_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe litigation z_pat_value_sci_topq_mix lmktvalue_cusd i.fiscal_year , absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

* VI sample
eststo: reghdfe litigation z_pat_value_kpss_svi lmktvalue_cusd i.fiscal_year  if merge_vi==3, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"

eststo: reghdfe litigation z_pat_value_hignvi_topq lmktvalue_cusd i.fiscal_year  if merge_vi==3, absorb(ipc   permno_adj) cluster(date)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)
estadd local Years = "Yes"
estadd local IPCs   = "No"
estadd local Firms = "Yes"


esttab using "Output\Table_1_v2_litigation_all_std_withipc.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
    drop(*.fiscal_year ) ///
	stats(mean_dv Years IPCs Firms ll N, ///
    label("Avg DV" "Year Fixed Effects" "4-digit IPC Fixed Effects" "Firm Fixed Effects" "Log Likelihood" "N") ///
    fmt(%9.3f %9.0gc %9.0gc %9.0gc %9.3f %12.0gc)) ///
    starlevels(* .10 ** .05 *** .01) label collabels(none) ///
    nomtitle ///
    mgroups("Litigation:All" "Litigation:VI Sample", pattern(1 0 0 0 0 1 0) ///
        prefix(\multicolumn{@span}{c}{) suffix(}) ///
        span erepeat(\cmidrule(lr){@span})) ///
    order(z_return z_pat_value_kpss z_pat_value_teamsize_topq_mix z_pat_value_foreign_mix z_pat_value_sci_topq_mix z_pat_value_kpss_svi z_pat_value_hignvi_topq lmktvalue_cusd )