*cd "C:\Users\jayho\OneDrive - Duke University\KPSS_Replication\ABF_PatValue_Research"
cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication"
********************************************************************************


***First part only create variables and key data****
*** all files used entity, code and year to merge
*
***cd "C:\Users\jayho\OneDrive - Duke University\ABF_PatValue_Research"

***gl OUT "C:\Users\jayho\OneDrive - Duke University\ABF_PatValue_Research\Output"

***gl IN "C:\Users\jayho\OneDrive - Duke University\ABF_PatValue_Research\Data"

clear all
 
********************************************************************************
*******************************Patent Level Science Indicators******************
********************************************************************************

use "Data\Patent_Science_Indicator.dta"



********************************************************************************
*******************************Add CPI******************************************
********************************************************************************
merge m:1 month year using "Data\CPI.dta",keepusing(cpi_index) 

keep if _merge==3

drop _merge




merge m:1 patent_num using "Data\patent_invt_indicator_final_v2.dta",keepusing(at_least_one_non_us all_nonusa_invt majority_nonusa_invt large_invt_team large_invt_team_topq   non_us_invt_67per) 

keep if _merge==3

drop _merge





label var at_least_one_non_us "Pat Foreign Invt (at least one)"
label var all_nonusa_invt "Pat Foreign Invt (All Non-USA)"
label var majority_nonusa_invt "Pat Foreign Invt (Majority Non-USA)"
label var large_invt_team "Team Size (above median)"
label var large_invt_team_topq "Team Size (topq)"

********************************************************************************
**********KPSS Full Sample***************
********************************************************************************

merge m:1 permno_adj date using "Data\patent_value_kpss_baseline.dta",keepusing(  patent_value_kpss mktvalue abnret npatents)
rename _merge merge_baseline

gen pat_value_kpss_cusd = patent_value_kpss/cpi_index
gen lpat_value_kpss= ln(1+pat_value_kpss_cusd*1000000) 


gen mktvalue_cusd = (mktvalue/cpi_index)/1000000
gen lmktvalue_cusd = ln(1+mktvalue_cusd*1000000)



label var pat_value_kpss_cusd "Pat Val: KPSS (const. usd)"
label var lpat_value_kpss "Log (Pat val KPSS (const. usd))"

label var mktvalue_cusd "Market cap (const. usd)"
label var lmktvalue_cusd "Log (Market cap (const. usd))"

label var abnret "Abnormal Return"

gen delta_marketcap = mktvalue_cusd*abnret

label var delta_marketcap "Abnormal Return * Market Cap"
********************************************************************************
**********Merging Patent value for science and Non-science patent***************
********************************************************************************
**Sample 1 - Science define as top 3 quartile 
merge m:1 permno_adj date using "Data\patent_Value_topq_fullSample.dta",keepusing(patents_topq_cscience_none patents_topq_cscience_mixed patents_topq_cscience_any npatents patent_value_kogan_fs patent_value_science_noadj patent_value_science_topq patent_value_nonscience_topq)

keep if _merge==3
rename _merge merge_topq_fs

unique permno_adj
unique patent_num

**
gen pat_value_sci_topq_mix = patent_value_science_topq if topq_cscience==1
replace pat_value_sci_topq_mix =patent_value_nonscience_topq if topq_cscience==0
**
gen pat_value_sci_topq_mix_cusd = pat_value_sci_topq_mix/cpi_index
gen lpatvalue_sci_topq_mix = ln(1+pat_value_sci_topq_mix_cusd*1000000) 
**
label var pat_value_sci_topq_mix_cusd "Pat Val: Sci NoSci (mixed)"
label var lpatvalue_sci_topq_mix "Log (Pat val Sci NoSci (mixed))"
**
gen pat_value_sci_topq_sep_cusd = patent_value_science_noadj/cpi_index
gen lpatvalue_sci_sep_topq = ln(1+pat_value_sci_topq_sep_cusd*1000000) 
**
label var pat_value_sci_topq_sep_cusd "Pat Val: Sci NoSci (sep)"
label var lpatvalue_sci_sep_topq "Log (Pat val: Sci NoSci (sep))"
**
********************************************************************************
**********Foreing Inventor***************
********************************************************************************
**Sample  Foreign Inventor is define if n
merge m:1 permno_adj date using "Data\patent_Value_allnonusa.dta",keepusing(pat_allnonusa_invt_any pat_allnonusa_invt_none patent_value_allnonusa_only patent_value_nononusa_only economic_value_allnonusa  patent_foreign patent_value_forinvt_noadj)

keep if _merge==3
rename _merge merge_foreign

unique permno_adj
unique patent_num

**
gen pat_value_foreign_mix = patent_value_allnonusa_only if all_nonusa_invt==1
replace pat_value_foreign_mix =patent_value_nononusa_only if all_nonusa_invt==0
**
gen pat_value_foreign_mix_cusd = pat_value_foreign_mix/cpi_index
gen lpatvalue_foreign_mix = ln(1+pat_value_foreign_mix_cusd*1000000) 
**
label var pat_value_foreign_mix_cusd "Pat Val: Foreing (no USA invt) (mixed)"
label var lpatvalue_foreign_mix "Log (Pat val: Foreing (no USA invt) (mixed))"
**
gen pat_value_foreign_sep_cusd = patent_value_forinvt_noadj/cpi_index
gen lpatvalue_foreign_sep_topq = ln(1+pat_value_foreign_sep_cusd*1000000) 
**
label var pat_value_foreign_sep_cusd "Pat Val: Foreing (no USA invt) (sep)"
label var lpatvalue_foreign_sep_topq "Log (Pat val: Foreing (no USA invt) (sep))"
**

********************************************************************************
**********Team Size***************
********************************************************************************
**Sample  Team size define using topq
merge m:1 permno_adj date using "Data\patent_Value_teamsize_topq.dta",keepusing(patent_value_largeteam_only patent_value_smallteam_only patent_value_team_noadj patent_largeteam_topq_any patent_largeteam_topq_none pat_largeteam_topq_mix pat_largeteam_topq)

keep if _merge==3
rename _merge merge_team_size

unique permno_adj
unique patent_num

**
gen pat_value_teamsize_topq_mix = patent_value_largeteam_only if large_invt_team_topq==1
replace pat_value_teamsize_topq_mix =patent_value_smallteam_only if large_invt_team_topq==0
**
gen pat_value_teamsize_topq_mix_cusd = pat_value_teamsize_topq_mix/cpi_index
gen lpatvalue_teamsize_topq_mix = ln(1+pat_value_teamsize_topq_mix_cusd*1000000) 
**
label var pat_value_teamsize_topq_mix_cusd "Pat Val: teamsize (topq) (mixed)"
label var lpatvalue_teamsize_topq_mix "Log (Pat val: teamsize (topq) (mixed))"
**
gen pat_value_teamsize_topq_sep_cusd = patent_value_team_noadj/cpi_index
gen lpatvalue_teamsize_sep_topq = ln(1+pat_value_teamsize_topq_sep_cusd*1000000) 
**
label var pat_value_teamsize_topq_sep_cusd "Pat Val: teamsize (topq) (sep)"
label var lpatvalue_teamsize_sep_topq "Log (Pat val: teamsize (topq) (sep))"
**

********************************************************************************
**********Vertical Integration***************
********************************************************************************
**Sample  Team size define using topq
merge m:1 permno_adj date using "Data\patent_Value_econ_value_kogan_verting.dta",keepusing(pa_high_vertinteg_high pa_high_vertinteg_low pa_topq_vertinteg_high pa_topq_vertinteg_low patent_value_kpss_svi patent_value_hignvi_med patent_value_hignvi_topq)

drop if _merge==2
rename _merge merge_vi

unique permno_adj
unique patent_num

**
gen pat_value_kpss_cusd_svi = patent_value_kpss_svi/cpi_index
gen lpat_value_kpss_cusd_svi= ln(1+pat_value_kpss_cusd_svi*1000000) 

label var pat_value_kpss_cusd_svi "Pat Val: KPSS (Verti Ineg.)"
label var lpat_value_kpss_cusd_svi "Log (Pat val KPSS (Verti Ineg.))"
**
sum patent_value_kpss_svi   
**
gen pat_value_hignvi_topqcusd = patent_value_hignvi_topq/cpi_index
gen lpat_value_hignvi_topq = ln(1+pat_value_hignvi_topqcusd*1000000) 
**
label var pat_value_hignvi_topqcusd "Pat Val: Verticle Integration (topq) (sep)"
label var lpat_value_hignvi_topq "Log (Pat val: Verticle Integration (topq) (sep))"




gen pat_value_hignvi_medcusd = patent_value_hignvi_med/cpi_index
gen lpat_value_hignvi_median = ln(1+pat_value_hignvi_medcusd*1000000) 
**
label var pat_value_hignvi_medcusd "Pat Val: Verticle Integration (median) (sep)"
label var lpat_value_hignvi_median "Log (Pat val: Verticle Integration (median) (sep))"
**









********************************************************************************
************************Adding OECD Patent Quality Indicatores******************
********************************************************************************

merge m:1 patent_num using "Data\patent_quality_oursample.dta",keepusing(patent_num tech_field many_field patent_scope family_size bwd_cits claims fwd_cits5 breakthrough generality originality radicalness renewal)
drop if _merge==2
drop _merge


gen log_fwd_cit5 =log(1+fwd_cits5)
gen log_family_size =log(1+family_size)
gen log_claims =log(claims)


label var family_size "Patent Family Size"
label var log_fwd_cit5 "log(1+ Patent forward cites)"
label var fwd_cits5 " Patent forward cites"
label var log_family_size "log(Family Size)"
label var breakthrough "I[Top 1\% of cited patents] x 100"
label var claims "Number of claims"
label var log_claims "log(Number of claims)"



********************************************************************************
************************Patent Value Using IID assumption***********************
********************************************************************************



merge m:1 permno_adj date using "Data\patent_Value_topq_fullSample_assum2.dta",keepusing(economic_value_kogan_assum2 )

drop _merge

unique permno_adj
unique patent_num

gen economic_value_kogan_assum2_cusd = economic_value_kogan_assum2/cpi_index
gen lpatvalue_kogan_assum2 = ln(1+economic_value_kogan_assum2_cusd*1000000) 



********************************************************************************
**********Merging Patent value for science and Non-science patent***************
********************************************************************************
merge m:1 patent_num using "Data\patent_reassignment.dta"

drop if _merge==2
drop _merge
label var reassign_dummy "Reassignment = 1"
label var trade_count "Num of Reassignment" 
replace reassign_dummy =0 if reassign_dummy==.
replace trade_count =0 if trade_count==.

gen topq_cscience_reassign = topq_cscience*reassign_dummy
label var topq_cscience_reassign "Science Dummy*Reassignment"
********************************************************************************
*******************Adding collateral_by_firstassignee***************************
********************************************************************************

merge m:1 patent_num using "Data\collateral_by_firstassignee.dta"

drop if _merge==2
drop _merge
label var collateral_by_firstassignee "Collateral=1"

replace collateral_by_firstassignee =100 if collateral_by_firstassignee==1
replace collateral_by_firstassignee =0 if collateral_by_firstassignee==.

gen topq_cscience_collateralfirst = topq_cscience*collateral_by_firstassignee
label var topq_cscience_collateralfirst "Science Dummy*Collateral"
********************************************************************************
************************Adding collateral_by_anyassignee************************
********************************************************************************
merge m:1 patent_num using "Data\collateral_by_anyassignee.dta"
drop if _merge==2
drop _merge

label var collateral_by_anyassignee "collateral(Any assignee)=1"
replace collateral_by_anyassignee =0 if collateral_by_anyassignee==.
replace collateral_by_anyassignee =100 if collateral_by_anyassignee==1

gen topq_cscience_collateralany = topq_cscience*collateral_by_anyassignee
label var topq_cscience_collateralany "Science Dummy*collateralfirst"



********************************************************************************
************************Family Size (Tri EPO, USPTO, JPO)***********************
********************************************************************************
merge m:1 patent_num using "Data\tech_big4.dta",keepusing(tech_big4)

drop if _merge==2
drop _merge



merge m:1 patent_num using "Data\PatentView\Trilateral_pat_oursample.dta",keepusing(trilateral_pat)
drop if _merge==2
drop _merge

label var trilateral_pat "Trilateral Pat"

********************************************************************************
************************Renewal Data****************************************
********************************************************************************

merge m:1 patent_num using "Data\PatentView\Renewal_Data_oursample.dta",keepusing(renewed_12th renewed_8th renewed_4th renewed_any)
drop if _merge==2
drop _merge

label var renewed_any "Renewd (at least once)"
label var renewed_12th "Renewd (Full term)"



********************************************************************************
************************Litigation Dummy***********************
********************************************************************************

merge m:1 patent_num using "Data\patents_litigation_dummy.dta",keepusing(litigation)
drop if _merge==2
drop _merge

replace litigation=0 if litigation==.







save "Data\Final_Patent_Level_Data.dta",replace




********************************************************************************
****************************************Appendix********************************
********************************************************************************

**Sample  Team size define using medain
merge m:1 permno_adj date using "Data\appendix\patent_Value_teamsize_median.dta",keepusing(patent_value_largeteam_m_only patent_value_smallteam_m_only patent_value_team_m_noadj patent_largeteam_any patent_largeteam_none pat_largeteam_mix)

keep if _merge==3
rename _merge merge_team_size_m

unique permno_adj
unique patent_num


label var large_invt_team "Large Team Dummy (median)"

**
gen pat_value_teamsize_m_mix = patent_value_largeteam_m_only if large_invt_team==1
replace pat_value_teamsize_m_mix =patent_value_smallteam_m_only if large_invt_team==0
**
gen pat_value_teamsize_m_mix_cusd = pat_value_teamsize_m_mix/cpi_index
gen lpatvalue_teamsize_m_mix = ln(1+pat_value_teamsize_m_mix_cusd*1000000) 
**
label var pat_value_teamsize_m_mix_cusd "Pat Val: teamsize (median) (mixed)"
label var lpatvalue_teamsize_m_mix "Log (Pat val: teamsize (median) (mixed))"
**
gen pat_value_teamsize_m_sep_cusd = patent_value_team_m_noadj/cpi_index
gen lpatvalue_teamsize_sep_m = ln(1+pat_value_teamsize_m_sep_cusd*1000000) 
**
label var pat_value_teamsize_m_sep_cusd "Pat Val: teamsize (median) (sep)"
label var lpatvalue_teamsize_sep_m "Log (Pat val: teamsize (median) (sep))"


 
********************************************************************************
**********Foreing Inventor***************
********************************************************************************
**Sample   Foreing inventor team two third of inventor from nonusa
merge m:1 permno_adj date using "Data\appendix\patent_Value_23nonusa.dta",keepusing(pat_67pernonusa_invt_any pat_67pernonusa_invt_none pat_67pernonusa_invt_mix patent_value_23nononusa_only patent_value_23allnonusa_only  patent_foreign_23 patent_value_23forinvt_noadj)

keep if _merge==3
rename _merge merge_foreign_23

unique permno_adj
unique patent_num


label var non_us_invt_67per "2/3 Inventor Non-USA"
**
gen pat_value_23foreign_mix = patent_value_23allnonusa_only if non_us_invt_67per==1
replace pat_value_23foreign_mix =patent_value_23nononusa_only if non_us_invt_67per==0
**
gen pat_value_23foreign_mix_cusd = pat_value_23foreign_mix/cpi_index
gen lpatvalue_23foreign_mix = ln(1+pat_value_23foreign_mix_cusd*1000000) 
**
label var pat_value_23foreign_mix_cusd "Pat Val:2/3 Foreign (no USA invt) (mixed)"
label var lpatvalue_23foreign_mix "Log (Pat val:2/3 Foreign (no USA invt) (mixed))"
**
gen pat_value_23foreign_sep_cusd = patent_value_23forinvt_noadj/cpi_index
gen lpatvalue_23foreign_sep_topq = ln(1+pat_value_23foreign_sep_cusd*1000000) 
**
label var pat_value_23foreign_sep_cusd "Pat Val: 2/3 Foreign (no USA invt) (sep)"
label var lpatvalue_23foreign_sep_topq "Log (Pat val: 2/3  Foreign (no USA invt) (sep))"
**




********************************************************************************
**********Merging Patent value for science and Non-science patent***************
********************************************************************************
**Sample 1 - Science define as top 3 quartile 
merge m:1 permno_adj date using "Data\appendix\patent_Value_median_fullSample.dta",keepusing(patents_cscience_adj_any patents_cscience_adj_none patents_cscience_adj_mixed npatents patent_value_kogan_fs patent_value_science_m_noadj patent_value_science_median patent_value_nonscience_median)

keep if _merge==3
rename _merge merge_median_fs

unique permno_adj
unique patent_num

label var cscience_adj "Science Dummy (median)"

**
gen pat_value_sci_median_mix = patent_value_science_median if cscience_adj==1
replace pat_value_sci_median_mix =patent_value_nonscience_median if cscience_adj==0
**
gen pat_value_sci_median_mix_cusd = pat_value_sci_median_mix/cpi_index
gen lpatvalue_sci_median_mix = ln(1+pat_value_sci_median_mix_cusd*1000000) 
**
label var pat_value_sci_median_mix_cusd "Pat Val: Sci NoSci (median) (mixed)"
label var lpatvalue_sci_median_mix "Log (Pat val Sci NoSci (median) (mixed))"
**
gen pat_value_sci_median_sep_cusd = patent_value_science_m_noadj/cpi_index
gen lpatvalue_sci_sep_median= ln(1+pat_value_sci_median_sep_cusd*1000000) 
**
label var pat_value_sci_median_sep_cusd "Pat Val: Sci NoSci (median) (sep)"
label var lpatvalue_sci_sep_median "Log (Pat val: Sci NoSci (median) (sep))"
**




save "Data\Final_Patent_Level_Data_appendix.dta",replace