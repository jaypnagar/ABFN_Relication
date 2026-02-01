*cd "C:\Users\jayho\OneDrive - Duke University\KPSS_Replication\ABF_PatValue_Research"

********************************************************************************

cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication"

gl OUT "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Output"

gl IN "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Data"

clear all

*log using "$OUT\analysis_regression_v2.log", replace

/* Part 2: Test for difference in value between patent categories */
*bcal create "Data\analyses", from(date) maxgap(1000) replace
use  "Data\analyses.dta", clear
*keep if permno_adj < 10100
keep if patentaward == 1
keep permno_adj bdate date patentaward npatents  patent_largeteam_any pat_67pernonusa_invt_any patents_cscience_adj_any 
sort permno_adj bdate
gen patent_award_date = date
egen event_id = group(patent_award_date)
sort permno_adj date
bys permno_adj: gen previous_award_date = patent_award_date[_n-1]
bys permno_adj: gen next_award_date = patent_award_date[_n+1]
gen dist = patent_award_date - previous_award_date
sum dist, d
replace patent_award_date = bdate
save "Data\ret.dta", replace

use  "Data\analyses.dta", clear
merge m:1 gvkey fyear using "Data\VertInteg_final_var.dta", keepusing(high_vertinteg topq_vertinteg vertinteg)
drop if _merge == 2
rename _merge merge_vertinteg
rename (high_vertinteg topq_vertinteg) (highvi topvi)

foreach var of varlist highvi topvi{
	gen pa_`var'_high = 0
	replace pa_`var'_high = 1 if patentaward == 1 & `var' == 1

	gen pa_`var'_low = 0
	replace pa_`var'_low = 1 if patentaward == 1 & `var' == 0
}

*keep if permno_adj < 10100
keep permno_adj sic naic date bdate fyear mktvalue ret abnret pa_highvi_high pa_highvi_low pa_topvi_high pa_topvi_low highvi topvi  merge_vertinteg
duplicates drop permno_adj bdate, force
xtset permno_adj bdate
sort permno_adj bdate
gen event_start = bdate - 2
gen event_end   = bdate 

rangejoin patent_award_date event_start event_end using "Data\ret.dta", by(permno_adj) keepusing(patentaward previous_award_date next_award_date npatents patent_largeteam_any pat_67pernonusa_invt_any patents_cscience_adj_any)
sort permno_adj bdate
duplicates drop permno_adj bdate, force

foreach var of varlist patent_award_date patentaward npatents patent_largeteam_any pat_67pernonusa_invt_any patents_cscience_adj_any pa_highvi_high{
	replace `var' = 0 if mi(`var')
}
gen event = patentaward

replace abnret = (abnret*100)
gen lmktvalue = ln(mktvalue)
xtset permno_adj bdate
gen mktvaluel = l1.mktvalue
gen lmktvaluel = l1.lmktvalue
compress
gen year = year(date)
gen sic2 = substr(sic, 1, 2)
destring sic2, gen(nsic2)
egen year_ind = group(year nsic2)
drop sic sic2 nsic2 mktvaluel lmktvalue mktvalue event_start event_end
save "Data\ret.dta", replace

use "Data\ret.dta", clear
gen abnret_adj = abnret 
replace abnret_adj = (abnret / 0.44) if event == 1
gen abnret_adj2 = abnret
replace abnret_adj2 = (abnret / 0.44) / npatents if event == 1


replace pa_topvi_high=. if merge_vertinteg==1
*******************Event Indicator for firm year


/*
bys permno year: egen fy_event = max(event)
bys permno year: egen fy_noevent = min(event)


bys permno year: egen fy_patents_topq_cscience_any = max(patents_topq_cscience_any)
bys permno year: egen fy_no_patents_topq_cscience_any = min(patents_topq_cscience_any) 

bys permno year: egen fy_pat_allnonusa_invt_any = max(pat_allnonusa_invt_any)
bys permno year: egen fy_no_pat_allnonusa_invt_any = min(pat_allnonusa_invt_any) 

bys permno year: egen fy_patent_largeteam_topq_any = max(patent_largeteam_topq_any)
bys permno year: egen fy_no_patent_largeteam_topq_any = min(patent_largeteam_topq_any) 

bys permno year: egen fy_pa_topvi_high = max(pa_topvi_high)
bys permno year: egen fy_no_pa_topvi_high = min(pa_topvi_high) 

*/



bys permno : egen fy_event = max(event)
bys permno : egen fy_noevent = min(event)


bys permno : egen fy_patents_topq_cscience_any = max(patents_cscience_adj_any)
bys permno : egen fy_no_patents_topq_cscience_any = min(patents_cscience_adj_any) 

bys permno : egen fy_pat_allnonusa_invt_any = max(pat_67pernonusa_invt_any)
bys permno : egen fy_no_pat_allnonusa_invt_any = min(pat_67pernonusa_invt_any) 

bys permno : egen fy_patent_largeteam_topq_any = max(patent_largeteam_any)
bys permno : egen fy_no_patent_largeteam_topq_any = min(patent_largeteam_any) 

bys permno : egen fy_pa_topvi_high = max(pa_highvi_high)
bys permno : egen fy_no_pa_topvi_high = min(pa_highvi_high) 



*************Baseline Regression*******************

sum patent_largeteam_any pat_67pernonusa_invt_any patents_cscience_adj_any pa_highvi_high





sum pa_topvi_hig,de


label var event "Patent day"

label var patent_largeteam_any "Patent day: Large Team Pat (median)"
*label var patent_largeteam_topq_none "Patent day: No Large Team Pat"

label var pat_67pernonusa_invt_any "Patent day: Foreign Invt Pat (2/3)"
*label var pat_allnonusa_invt_none "Patent day: No Foreign Invt Pat"

label var patents_cscience_adj_any "Patent day: Science Pat (median)"
*label var patents_topq_cscience_none "Patent day: No Science Pat"


label var pa_highvi_high "Patent day: High VI Firm (median)"
*label var pa_topq_vertinteg_low "Patent day: Low VI Firm"




est clear
eststo sumstats: qui estpost sum abnret event patent_largeteam_any pat_67pernonusa_invt_any patents_cscience_adj_any pa_highvi_high, d
esttab sumstats using "Output\table_1b_summarystats_return_all.tex", cells("count(fmt(0)) mean(fmt(3)) sd( fmt(3)) p10(fmt(3)) p50(fmt(3))  p90(fmt(3))") nonumber nomtitle noobs label collabels("Obs." "Mean" "Std. Dev." "10\%" "50\%" "90\%") replace


eststo clear
****************************************************************************************
*xtset permno_adj 

	   
	   
est clear
* 1. ppmlhdfe
* start with no controls, add quality controls

eststo:reghdfe abnret event patent_largeteam_any lmktvaluel npatents if fy_event == 1 & fy_noevent == 0 & fy_patent_largeteam_topq_any == 1 & fy_no_patent_largeteam_topq_any == 0, absorb(permno_adj bdate) cluster(permno)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Firms = "Yes"
estadd local date = "Yes"

eststo:reghdfe abnret event pat_67pernonusa_invt_any lmktvaluel npatents if fy_event == 1 & fy_noevent == 0 & fy_pat_allnonusa_invt_any == 1 & fy_no_pat_allnonusa_invt_any == 0, absorb(permno_adj bdate) cluster(permno)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Firms = "Yes"
estadd local date = "Yes"

eststo:reghdfe abnret event patents_cscience_adj_any lmktvaluel npatents if fy_event == 1 & fy_noevent == 0 & fy_patents_topq_cscience_any == 1 & fy_no_patents_topq_cscience_any == 0, absorb(permno_adj bdate) cluster(permno)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Firms = "Yes"
estadd local date = "Yes"

eststo:reghdfe abnret event pa_highvi_high lmktvaluel npatents if fy_event == 1 & fy_noevent == 0 & fy_pa_topvi_high == 1 & fy_no_pa_topvi_high == 0 &  merge_vertinteg==3, absorb(permno_adj bdate) cluster(permno)
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)  
estadd local Firms = "Yes"
estadd local date = "Yes"


esttab using "Output\Table_Appendix_AbnoralReturnAsDep.tex", replace cells(b(star fmt(%12.4f)) se(par)) ///
	stats(mean_dv  Firms date r2 N, ///
	label("Avg DV" "Firm Fixed Effects" "Business Date" "R^2" "N") ///
	 fmt( %9.3f %9.0gc %9.0gc  %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Abnormal Return" , pattern(1 0 0  0  ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
       order(event patent_largeteam_any pat_67pernonusa_invt_any patents_cscience_adj_any pa_highvi_high mktvaluel npatents)
	   	
		
		
	
	
	
	
	
*log close		