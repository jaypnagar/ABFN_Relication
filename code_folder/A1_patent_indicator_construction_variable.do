/* Set Directory */

/*Elia  cd "C:\Users\ef122\Duke University\Jay Prakash Nagar - ABF_PatValue_Research"*/
/*Jay*/  cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication" 

********************************************************************************
***********************Patent IPC Class*****************************************
********************************************************************************
clear all
insheet using "Data\g_ipc_at_issue.tsv", tab
gen ipc_4_digit = section +ipc_class+ subclass
keep if ipc_sequence==0
rename patent_id  patent_num
keep patent_num ipc_4_digit

gen ipc_2_digit = substr(ipc_4_digit, 1, 2)
gen ipc_3_digit = substr(ipc_4_digit, 1, 3)

save "Data\patent_ipc.dta", replace



clear all
********************************************************************************
*******************************Patent Level Science Indicators******************
********************************************************************************

use "Data\patents_1980_2021_UO_with_citations_to_science.dta", clear
compress
rename (publn_nr) (patent_num)

*****merge patent IPC class at 4 digit level*******
merge m:1 patent_num using "Data\patent_ipc.dta"
keep if _merge==3
drop _merge
******************************************************
order permno_adj fiscal_year grant_year patent_num filing_date patent_date

unique patent_num
***** Replace the cites_science to zero if confscore is less than 5
***** We should not drop, becuase it will drop the patent all together


replace cites_science =0 if confscore < 5 
bys permno_adj grant_year patent_num: egen cscience = max(cites_science)
bys permno_adj grant_year patent_num: egen cscience_count = sum(cites_science)


gen cites_science_conf9 =cites_science
replace cites_science_conf9 =0 if confscore < 9 
bys permno_adj grant_year patent_num: egen cscience_conf9 = max(cites_science_conf9)
bys permno_adj grant_year patent_num: egen cscience_count_conf9 = sum(cites_science_conf9)


******************************************************
gen cites_intext =0
replace cites_intext =1 if wherefound=="bodyonly"|wherefound=="both"
bys permno_adj grant_year patent_num: egen cscience_intext = max(cites_intext)
bys permno_adj grant_year patent_num: egen cscience_count_intext = sum(cites_intext)
******************************************************


keep /*gvkey ngvkey*/ permno_adj fiscal_year grant_year patent_num filing_date patent_date cscience cscience_count ipc_4_digit cscience_conf9 cscience_count_conf9 cscience_intext cscience_count_intext
duplicates drop


********Basic three indicators********
bys permno_adj patent_date: gen npatents = _N
bys permno_adj patent_date: egen npatents_cscience = sum(cscience)
gen prop_cscience = npatents_cscience / npatents

bys permno_adj patent_date: egen npatents_cscience_intext = sum(cscience_intext)
gen prop_cscience_intext = npatents_cscience_intext / npatents

bys permno_adj patent_date: egen npatents_cscience_conf9 = sum(cscience_conf9)
gen prop_cscience_conf9 = npatents_cscience_conf9 / npatents



sort permno_adj fiscal_year patent_date patent_num  ipc_4_digit
bysort grant_year ipc_4_digit: egen cscience_count_avg_yr = mean(cscience_count) if cscience == 1
bysort grant_year ipc_4_digit: egen cscience_count_med_yr = median(cscience_count) if cscience == 1


***Greater than Median citation
gen cscience_adj = 0
replace cscience_adj = 1 if cscience_count > cscience_count_med_yr
bys permno_adj patent_date: egen npatents_cscience_adj = sum(cscience_adj)
gen prop_cscience_adj = npatents_cscience_adj / npatents


tab cscience_adj if fiscal_year<=2005
tab cscience_adj if fiscal_year>2005

***top 3 quartile
bysort grant_year ipc_4_digit: astile quartile_npl_yr = cscience_count if cscience == 1, nq(4)
gen topq_cscience = 0
replace topq_cscience = 1 if cscience == 1 & quartile_npl_yr>1

bys permno_adj patent_date: egen npatents_topq_cscience = sum(topq_cscience)
gen prop_topq_cscience = npatents_topq_cscience / npatents

***only top quartile
gen top4q_cscience = 0
replace top4q_cscience = 1 if cscience == 1 & quartile_npl_yr==4

bys permno_adj patent_date: egen npatents_top4q_cscience = sum(top4q_cscience)
gen prop_top4q_cscience = npatents_top4q_cscience / npatents


***
bysort grant_year ipc_4_digit: astile quartile_npl_yr_conf9 = cscience_count_conf9 if cscience_conf9 == 1, nq(4)
gen topq_cscience_conf9 = 0
replace topq_cscience_conf9 = 1 if cscience_conf9 == 1 & quartile_npl_yr_conf9>1
bys permno_adj patent_date: egen npatents_topq_cscience_conf9 = sum(topq_cscience_conf9)
gen prop_topq_csciencee_conf9 = npatents_topq_cscience_conf9 / npatents




tab topq_cscience if fiscal_year<=2005
tab topq_cscience if fiscal_year>2005


gen y = substr(patent_date, 1, 4)
destring y, gen(year)
gen m = substr(patent_date, 6, 2)
destring m, gen(month)
gen d = substr(patent_date, 9, 2)
destring d, gen(day)
gen granteddate = mdy(month, day, year)
format granteddate %td



gen date = granteddate
drop if fiscal_year>2019


save "Data\Patent_Science_Indicator.dta", replace





********************************************************************************
*******************************Add Patent Quality Indicator*********************
********************************************************************************

clear all


import delimited "Data\202401_OECD_PATENT_QUALITY_USPTO_INDIC.txt", delimiter("|")

gen publn_nr = real(subinstr(pub_nbr, "US", "", .))

destring publn_nr, replace

duplicates drop publn_nr, force
save "Data\patent_quality.dta", replace



use "Data\patents_1980_2021_UO_with_citations_to_science.dta", clear
compress
rename (publn_nr) (patent_num)

keep patent_num grant_year patent

gen publn_nr= patent_num

duplicates drop

destring publn_nr, replace

duplicates drop publn_nr, force
merge 1:1 publn_nr using "Data\patent_quality.dta"

keep if _merge ==3
drop _merge

save "Data\patent_quality_oursample.dta", replace


********************************************************************************
*******************************CPI Data*****************************************
********************************************************************************
clear all


import delimited "Data\CPIAUCSL.csv", delimiter(",")


gen y = substr(date, 1, 4)
destring y, gen(year)
gen m = substr(date, 6, 2)
destring m, gen(month)

gen cpi_index = cpiaucsl/100

save "Data\CPI.dta", replace

collapse (mean) cpi_index, by(year)

save "Data\CPI_year.dta", replace



clear all
********************************************************************************
****************************Team charaterstics**********************************
********************************************************************************

import delimited "Data\patent_invt_indicator.csv", delimiter(",")
tostring pat_num, gen(patent_num)

*****merge patent IPC class at 4 digit level*******
merge m:1 patent_num using "Data\patent_ipc.dta"
keep if _merge==3
drop _merge
******************************************************
save "Data\patent_invt_indicator.dta", replace



clear all
use "Data\patents_1980_2021_UO_with_citations_to_science.dta", clear
compress
rename (publn_nr) (patent_num)

keep patent_num grant_year permno_adj patent_date

duplicates drop permno_adj patent_num, force
merge m:1 patent_num using "Data\patent_invt_indicator.dta"
keep if _merge==3
drop _merge

gen multi_invt_team =0 if total_person_id==1
replace multi_invt_team=1 if multi_invt_team==.



bysort grant_year ipc_4_digit: egen team_size_med = median(total_person_id) 
gen large_invt_team = 0
replace large_invt_team = 1 if total_person_id > team_size_med

***top 3 quartile
bysort grant_year ipc_4_digit: astile quartile_total_person_id  = total_person_id, nq(4)
gen large_invt_team_topq = 0
replace large_invt_team_topq = 1 if  quartile_total_person_id>3



gen share_invt = non_us_count/(us_count+non_us_count)

gen non_us_invt_75per=0
replace non_us_invt_75per = 1 if share_invt>=0.75 

gen non_us_invt_67per=0
replace non_us_invt_67per = 1 if share_invt>=2/3 






keep permno_adj patent_date patent_num total_person_id us_count non_us_count majority_nonusa_invt all_nonusa_invt at_least_one_non_us large_invt_team large_invt_team_topq grant_year multi_invt_team non_us_invt_75per non_us_invt_67per

preserve
collapse (mean) at_least_one_non_us all_nonusa_invt majority_nonusa_invt non_us_invt_75per, by(grant_year)

twoway (line at_least_one_non_us grant_year, lcolor(blue) lwidth(medthick)) ///
       (line majority_nonusa_invt grant_year, lcolor(red) lwidth(medthick)) ///
       (line all_nonusa_invt grant_year, lcolor(green) lwidth(medthick)), ///
       xlabel(, angle(45)) ///
       ylabel(, grid) ///
       title("Mean of Non-USA Inventor Variables Over Grant Year") ///
       xtitle("Grant Year") ///
       ytitle("Mean Value") ///
       legend(order(1 "At Least One Non-US" 2 "Majority Non-USA" 3 "All Non-USA") ///
       position(6) ring(0) size(small))

restore
******************************************************

gen y = substr(patent_date, 1, 4)
destring y, gen(year)
gen m = substr(patent_date, 6, 2)
destring m, gen(month)
gen d = substr(patent_date, 9, 2)
destring d, gen(day)
gen granteddate = mdy(month, day, year)
format granteddate %td

save "Data\patent_invt_indicator_final.dta", replace


gen patent_count =1

collapse (sum) patent_count at_least_one_non_us all_nonusa_invt majority_nonusa_invt large_invt_team large_invt_team_topq multi_invt_team non_us_invt_75per non_us_invt_67per,by(permno_adj granteddate)


gen date = granteddate


save "Data\Firm_patent_invt_date.dta", replace


clear 

use "Data\patent_invt_indicator_final.dta", clear

duplicates drop patent_num, force

save "Data\patent_invt_indicator_final_v2.dta", replace
