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

use "Data\Final_Patent_Level_Data.dta"

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