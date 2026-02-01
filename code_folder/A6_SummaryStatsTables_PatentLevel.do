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


replace abnret_d02 = abnret_d02*100
label var abnret_d02 "Abnormal return on day t(0-2)"

* Other variables
gen abnret_d02_adj = abnret_d02 / npatents
egen ipc = group(ipc_4_digit)
replace breakthrough = breakthrough*100

replace litigation = litigation*100

label var trilateral_pat "I[Trilateral Patent]"
label var litigation "I[Litigation]x100"


replace renewed_12th=. if fiscal_year<1992 |fiscal_year>2012

*******************winsorize**************************

winsor2 abnret_d02 pat_value_kpss_cusd  pat_value_teamsize_topq_sep_cusd pat_value_foreign_mix_cusd pat_value_foreign_sep_cusd pat_value_teamsize_topq_mix_cusd pat_value_sci_topq_sep_cusd pat_value_sci_topq_mix_cusd    pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd, cuts(1 99) replace
*******************winsorize**************************

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
label var  pat_value_foreign_mix_cusd "Pat Val: Foreign (mixed)"
 
label var pat_value_sci_topq_sep_cusd "Pat Val: Sci\&NoSci (sep)"
label var pat_value_sci_topq_mix_cusd "Pat Val: Sci\&NoSci (mixed)"

label var pat_value_kpss_cusd_svi "Pat Val: KPSS (VI)"
label var pat_value_hignvi_topqcusd "Pat Val: Vertical Integration (sep)"







/* we can either control for npatents, or adjust returns by dividing by npatents */

est clear
eststo sumstats: qui estpost sum  large_invt_team_topq all_nonusa_invt topq_cscience  pa_topq_vertinteg_high fwd_cits5 breakthrough renewed_12th reassign_dummy trilateral_pat litigation , d
esttab sumstats using "Output\table_1a_summarystats_depvar.tex", cells("count(fmt(0)) mean(fmt(3)) sd( fmt(3)) p10(fmt(3)) p50(fmt(3))  p90(fmt(3))") nonumber nomtitle noobs label collabels("Obs." "Mean" "Std. Dev." "10\%" "50\%" "90\%") replace







est clear
eststo sumstats: qui estpost sum  pat_value_kpss_cusd pat_value_teamsize_topq_mix_cusd pat_value_teamsize_topq_sep_cusd  pat_value_foreign_mix_cusd pat_value_foreign_sep_cusd  pat_value_sci_topq_mix_cusd pat_value_sci_topq_sep_cusd     pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd abnret_d02, d
esttab sumstats using "Output\table_1b_summarystats_depvar_patvalue.tex", cells("count(fmt(0)) mean(fmt(3)) sd( fmt(3)) p10(fmt(3)) p50(fmt(3))  p90(fmt(3))") nonumber nomtitle noobs label collabels("Obs." "Mean" "Std. Dev." "10\%" "50\%" "90\%") replace







********************************************************************************
**********Inventor Team Size (based on topq) ***************
********************************************************************************
gen large_invt_team_other = 0 
replace large_invt_team_other =1 if large_invt_team_topq==0

eststo clear
eststo large_invt_team_topq: quietly estpost summarize pat_value_kpss_cusd pat_value_teamsize_topq_mix_cusd pat_value_teamsize_topq_sep_cusd abnret_d02 if large_invt_team_topq==1
eststo large_invt_team_other: quietly  estpost summarize pat_value_kpss_cusd pat_value_teamsize_topq_mix_cusd pat_value_teamsize_topq_sep_cusd abnret_d02 if large_invt_team_topq==0

eststo diff: quietly estpost ttest pat_value_kpss_cusd pat_value_teamsize_topq_mix_cusd pat_value_teamsize_topq_sep_cusd abnret_d02, by(large_invt_team_topq)

esttab large_invt_team_topq large_invt_team_other diff using "Output\table_2_panel_c.tex", replace cells("mean(pattern(1 1 0) fmt(3)) sd(pattern(1 1 0)) b(star pattern(0 0 1) fmt(3))") label order()

********************************************************************************
**********Foreing Domestic: All Non-USA inventors ***************
********************************************************************************
gen all_nonusa_invt_other = 0 
replace all_nonusa_invt_other =1 if all_nonusa_invt==0

eststo clear
eststo all_nonusa_invt: quietly estpost summarize pat_value_kpss_cusd pat_value_foreign_mix_cusd pat_value_foreign_sep_cusd abnret_d02 if all_nonusa_invt==1
eststo all_nonusa_invt_other: quietly  estpost summarize pat_value_kpss_cusd pat_value_foreign_mix_cusd pat_value_foreign_sep_cusd abnret_d02 if all_nonusa_invt==0

eststo diff: quietly estpost ttest pat_value_kpss_cusd pat_value_foreign_mix_cusd pat_value_foreign_sep_cusd abnret_d02, by(all_nonusa_invt)

esttab all_nonusa_invt all_nonusa_invt_other diff using "Output\table_2_panel_b.tex", replace cells("mean(pattern(1 1 0) fmt(3)) sd(pattern(1 1 0)) b(star pattern(0 0 1) fmt(3))") label order()




********************************************************************************
**********Merging Patent value for science and Non-science patent***************
********************************************************************************
gen topq_other = 0 
replace topq_other =1 if topq_cscience==0
eststo clear
eststo topq_cscience: quietly estpost summarize pat_value_kpss_cusd pat_value_sci_topq_mix_cusd pat_value_sci_topq_sep_cusd abnret_d02 if topq_cscience==1
eststo topq_other: quietly  estpost summarize pat_value_kpss_cusd pat_value_sci_topq_mix_cusd pat_value_sci_topq_sep_cusd abnret_d02 if topq_cscience==0

eststo diff: quietly estpost ttest pat_value_kpss_cusd pat_value_sci_topq_mix_cusd pat_value_sci_topq_sep_cusd abnret_d02, by(topq_cscience)

esttab topq_cscience topq_other diff using "Output\table_2_panel_a.tex", replace cells("mean(pattern(1 1 0) fmt(3)) sd(pattern(1 1 0)) b(star pattern(0 0 1) fmt(3))") label order()




********************************************************************************
**********Vertical Integration ***************
********************************************************************************
keep if  merge_vi==3


est clear
eststo sumstats: qui estpost sum pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd pa_topq_vertinteg_high abnret_d02, d
esttab sumstats using "Output\table_1_panel_b.tex", cells("count(fmt(0)) mean(fmt(3)) sd( fmt(3)) p10(fmt(3)) p50(fmt(3))  p90(fmt(3))") nonumber nomtitle noobs label collabels("Obs." "Mean" "Std. Dev." "10\%" "50\%" "90\%") replace


eststo clear
eststo pa_topq_vertinteg_high: quietly estpost summarize pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd abnret_d02 if pa_topq_vertinteg_high==1
eststo pa_topq_vertinteg_low: quietly  estpost summarize pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd abnret_d02 if pa_topq_vertinteg_high==0

eststo diff: quietly estpost ttest pat_value_kpss_cusd_svi pat_value_hignvi_topqcusd abnret_d02, by(pa_topq_vertinteg_high)

esttab pa_topq_vertinteg_high pa_topq_vertinteg_low diff using "Output\table_2_panel_d.tex", replace cells("mean(pattern(1 1 0) fmt(3)) sd(pattern(1 1 0)) b(star pattern(0 0 1) fmt(3))") label order()




