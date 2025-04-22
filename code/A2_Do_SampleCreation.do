/* Set Directory */

/*Elia  cd "C:\Users\ef122\Duke University\Jay Prakash Nagar - ABF_PatValue_Research"*/
/*Jay  cd "C:\Users\jayho\OneDrive - Duke University\ABF_PatValue_Research" */


clear all

/* Identify firm-grant date for returns */
use "Data\patents_1980_2021_UO_with_citations_to_science.dta", clear
keep permno_adj patent_date
duplicates drop
gen y = substr(patent_date, 1, 4)
destring y, gen(year)
gen m = substr(patent_date, 6, 2)
destring m, gen(month)
gen d = substr(patent_date, 9, 2)
destring d, gen(day)
gen granteddate = mdy(month, day, year)
format granteddate %td
keep /*gvkey ngvkey*/ permno_adj granteddate 
duplicates drop
sort permno_adj granteddate
export excel using "data\patentawards.xlsx", firstrow(variables) replace

/********/
* RUN SAS
/********/

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

unique patent_num
tab cscience if fiscal_year<=2005
tab cscience if fiscal_year>2005


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

keep /*gvkey ngvkey*/ permno_adj granteddate npatents npatents_cscience prop_cscience npatents_cscience_adj  prop_cscience_adj  npatents_topq_cscience prop_topq_cscience  npatents_top4q_cscience prop_top4q_cscience npatents_cscience_intext  prop_cscience_intext   npatents_topq_cscience_conf9 prop_topq_csciencee_conf9

duplicates drop
gen date = granteddate





save "Data\kogan_patents.dta", replace

use "Data\ret_patents.dta"
duplicates drop permno_adj date, force
merge 1:1 permno_adj date using "Data\kogan_patents.dta"
drop if _merge == 2
keep if fyear > 1979
save "Data\patentsample.dta", replace




*********************************************************************************
*********************************************************************************
*********************************************************************************

clear all
use "Data\patentsample.dta", clear // 13,475,199 observations
rename *, lower
keep permno_adj gvkey datadate fyear naics

gen naics_2digit = substr(naics, 1, 2)
destring naics_2digit, replace

* Create month-year variable (formatted as YYYY-MM)
gen month_year = mofd(datadate)
format month_year %tm
label variable month_year "Month-Year (YYYY-MM)"
duplicates drop permno_adj gvkey datadate fyear, force
save "Data\sample_gvkey.dta", replace
keep permno_adj gvkey fyear naics_2digit
duplicates drop permno_adj gvkey fyear, force
save "Data\sample_gvkey_year.dta", replace


***************************************************************************
**************Vertical Integration******************************************
***************************************************************************
clear all 
import delimited "Data\VertInteg.txt", delimiter("\t")
gen fyear = year
tab year
tostring gvkey, gen(gvkey_str)
rename gvkey gvkey_int
rename gvkey_str gvkey
replace gvkey = string(gvkey_int, "%06.0f")
save "Data\VertInteg.dta", replace

clear all
use "Data\sample_gvkey_year.dta"
merge m:1 gvkey fyear using "Data\VertInteg.dta"
keep if _merge ==3
drop _merge
 
tab year

bysort fyear naics_2digit:egen median_vertinteg = median(vertinteg)
gen high_vertinteg=0
replace high_vertinteg = vertinteg > median_vertinteg

bysort fyear naics_2digit: astile quartile_vertinteg  = vertinteg, nq(4)
gen topq_vertinteg = 0
replace topq_vertinteg = 1 if  quartile_vertinteg>3


sum high_vertinteg topq_vertinteg vertinteg

save "Data\VertInteg_final_var.dta", replace
*********************************************************************************
*********************************************************************************
*********************************************************************************



********************************************************************************
* Calculate variables for Sharon
********************************************************************************
use "Data\patentsample.dta", clear // 13,475,199 observations
rename *, lower
 
drop _merge
merge m:1 permno_adj granteddate using "Data\Firm_patent_invt_date.dta" 
drop if _merge ==2
drop _merge
 
 
* The data are unique at the gvkey-date level, but we need them to be unique at the permno_adj-date level
gsort permno_adj date
gduplicates tag permno_adj date, gen(dup)
tab dup

* Set business calendar
order permno_adj date // observations are unique at this level
bcal create mycal, from(date) maxgap(30) generate(bdate) replace // creates a business calendar file based on dates in variable "date"; business holidays are inferred from gaps in variable "date"
bcal check

foreach var of varlist npatents npatents_cscience prop_cscience {
    replace `var' = 0 if mi(`var')
}

gen patentaward = 0
replace patentaward = 1 if npatents > 0 & !mi(npatents)

* Identify firms that received at least one patent
bysort permno_adj: egen patent_firm = max(patentaward)
unique permno_adj if patent_firm // 4,230
unique permno_adj // 4,387

* * * * * * * * * 
gen pat_atonenonus_invt_any =0
replace pat_atonenonus_invt_any = 1 if at_least_one_non_us > 0 & at_least_one_non_us !=. & patentaward == 1 

gen pat_atonenonus_invt_mix =0 
replace pat_atonenonus_invt_mix = 1 if at_least_one_non_us<npatents  & at_least_one_non_us >0 & at_least_one_non_us !=. & npatents > 1 

gen pat_atonenonus_invt_none =0
replace pat_atonenonus_invt_none = 1 if at_least_one_non_us == 0 & at_least_one_non_us !=. & patentaward == 1 

* * * * * * * * * 

* * * * * * * * * 
gen pat_allnonusa_invt_any =0
replace pat_allnonusa_invt_any = 1 if all_nonusa_invt > 0 & all_nonusa_invt !=. & patentaward == 1 

gen pat_allnonusa_invt_mix =0 
replace pat_allnonusa_invt_mix = 1 if all_nonusa_invt<npatents  & all_nonusa_invt >0 & all_nonusa_invt !=. & npatents > 1 

gen pat_allnonusa_invt_none =0
replace pat_allnonusa_invt_none = 1 if all_nonusa_invt == 0 & all_nonusa_invt !=. & patentaward == 1 
* * * * * * * * * 


* * * * * * * * * 
gen pat_majornonusa_invt_any =0
replace pat_majornonusa_invt_any = 1 if majority_nonusa_invt > 0 & majority_nonusa_invt !=. & patentaward == 1 

gen pat_majornonusa_invt_mix =0 
replace pat_majornonusa_invt_mix = 1 if majority_nonusa_invt<npatents  & majority_nonusa_invt >0 & majority_nonusa_invt !=. & npatents > 1 


gen pat_majornonusa_invt_none =0
replace pat_majornonusa_invt_none = 1 if majority_nonusa_invt == 0 & majority_nonusa_invt !=. & patentaward == 1 
* * * * * * * * * 


* * * * * * * * * 
gen patent_largeteam_any =0
replace patent_largeteam_any = 1 if large_invt_team>0 & large_invt_team !=. & patentaward == 1 

gen pat_largeteam_mix =0 
replace pat_largeteam_mix = 1 if large_invt_team<npatents  & large_invt_team >0 & large_invt_team !=. & npatents > 1 


gen patent_largeteam_none =0
replace patent_largeteam_none = 1 if large_invt_team <=0 & large_invt_team !=. & patentaward == 1 
* * * * * * * * * 



gen patent_largeteam_topq_any =0
replace patent_largeteam_topq_any = 1 if large_invt_team_topq>0 & large_invt_team !=. & patentaward == 1 

gen pat_largeteam_topq_mix =0 
replace pat_largeteam_topq_mix = 1 if large_invt_team_topq<npatents  & large_invt_team_topq >0 & large_invt_team_topq !=. & npatents > 1 

gen patent_largeteam_topq_none =0
replace patent_largeteam_topq_none = 1 if large_invt_team_topq <=0 & large_invt_team !=. & patentaward == 1 






sum pat_atonenonus_invt_any pat_allnonusa_invt_any pat_majornonusa_invt_any pat_atonenonus_invt_none pat_allnonusa_invt_none patent_largeteam_any patent_largeteam_none patent_largeteam_topq_any patent_largeteam_topq_none if patentaward == 1 




*********************************************************************************
gen patents_cscience_none = 0
replace patents_cscience_none = 1 if prop_cscience == 0 & patentaward == 1

gen patents_cscience_any = 0
replace patents_cscience_any = 1 if prop_cscience > 0 & patentaward == 1

gen patents_cscience_mixed = 0
replace patents_cscience_mixed = 1 if patentaward == 1 & prop_cscience != 0 & prop_cscience != 1
gen patents_cscience_only = 0
replace patents_cscience_only = 1 if prop_cscience == 1
gen patents_cscience_adj_none = 0
replace patents_cscience_adj_none = 1 if prop_cscience_adj == 0 & patentaward == 1


gen patents_cscience_adj_any = 0
replace patents_cscience_adj_any = 1 if prop_cscience_adj > 0 & patentaward == 1
gen patents_cscience_adj_mixed = 0
replace patents_cscience_adj_mixed = 1 if patentaward == 1 & prop_cscience_adj != 0 & prop_cscience_adj != 1
gen patents_cscience_adj_only = 0
replace patents_cscience_adj_only = 1 if prop_cscience_adj == 1



gen patents_topq_cscience_any = 0
replace patents_topq_cscience_any = 1 if prop_topq_cscience > 0 & patentaward == 1
gen patents_topq_cscience_mixed = 0
replace patents_topq_cscience_mixed = 1 if patentaward == 1 & prop_topq_cscience != 0 & prop_topq_cscience != 1
gen patents_topq_cscience_none = 0
replace patents_topq_cscience_none = 1 if prop_topq_cscience == 0 & patentaward == 1


gen patents_topq_cscience_conf9_any = 0
replace patents_topq_cscience_conf9_any = 1 if prop_topq_csciencee_conf9 > 0 & patentaward == 1
gen patents_topq_cscience_conf9_none = 0
replace patents_topq_cscience_conf9_none = 1 if prop_topq_csciencee_conf9 == 0 & patentaward == 1
gen patents_topq_cscience_conf9_mix = 0
replace patents_topq_cscience_conf9_mix = 1 if patentaward == 1 & prop_topq_csciencee_conf9 != 0 & prop_topq_csciencee_conf9 != 1




gen patents_cscience_intext_any = 0
replace patents_cscience_intext_any = 1 if prop_cscience_intext > 0 & patentaward == 1
gen patents_cscience_intext_none = 0
replace patents_cscience_intext_none = 1 if prop_cscience_intext == 0 & patentaward == 1



gen patents_top4q_cscience_any = 0
replace patents_top4q_cscience_any = 1 if prop_top4q_cscience > 0 & patentaward == 1
gen patents_top4q_cscience_none = 0
replace patents_top4q_cscience_none = 1 if prop_top4q_cscience == 0 & patentaward == 1


gen check_any = 0
replace check_any = 1 if patents_topq_cscience_any==1 &  patents_cscience_intext_any==1 & patentaward == 1
gen check_none = 0
replace check_none = 1 if patents_topq_cscience_any == 0 & patentaward == 1





* Identify firms that received at least one non-science citing and one science citing patent
*bysort permno_adj: egen patentscience_firm = max(patents_cscience_only)
bysort permno_adj: egen patentscience_firm = max(patents_cscience_any)
bysort permno_adj: egen patentnoscience_firm = max(patents_cscience_none)



*bysort permno_adj: egen patentscience_adj_firm = max(patents_cscience_adj_only)
bysort permno_adj: egen patentscience_adj_firm = max(patents_cscience_adj_any)
bysort permno_adj: egen patentnoscience_adj_firm = max(patents_cscience_adj_none)

bysort permno_adj: egen patenttopq_cscience_firm = max(patents_topq_cscience_any)
bysort permno_adj: egen patentnotopq_cscience_firm = max(patents_topq_cscience_none)


* Identify other variables
/*
egen temp = mean(prop_median_cscience)
gen patents_cscience_high = 0
replace patents_cscience_high = 1 if prop_median_cscience > temp & !mi(prop_median_cscience) & patentaward == 1
gen patents_cscience_low = 0
replace patents_cscience_low = 1 if prop_median_cscience <= temp & !mi(prop_median_cscience) & patentaward == 1
drop temp
label var npatents "Number of patents awarded"
label var npatents_cscience "Number of patents that cite science awarded"
label var prop_cscience "Proportion of patents awarded that cite science"
label var patents_cscience_only "Only patents that cite science awarded"
label var patents_cscience_none "Only patents that do not cite science awarded"
label var patents_mixed "Only mixed patent awards"
*/

gen turnover = 100 * vol / (shrout * 1000) // express turnover as a percentage; Kogan's median turnover was 1.29%; ours is 0.30% for all contracting firms over 1984-2015
summ turnover, det
order permno_adj bdate
gsort permno_adj bdate
xtset permno_adj bdate
gen abnret = ret - mktret // 271,667 missing values generated
label var abnret "Abnormal return on day t"
rangestat (sum) abnret_d02 = abnret, by(permno_adj) interval(bdate 0 2) // 3,325 missing values generated
label var abnret_d02 "Abnormal return on day t+1"
gen abnretsq_d02 = abnret_d02 * abnret_d02
gen ln_abnretsq_d02 = ln(abnretsq_d02) // 273,085 missing values generated
bysort permno_adj fyear: egen vol_ar_fy = mean(abnretsq_d02)
gen mktvalue = prc * shrout * 1000


keep permno_adj bdate date gvkey datadate fyear mktvalue sic naics ret abnret abnret_d02 abnretsq_d02 ln_abnretsq_d02 vol_ar_fy patentaward npatents patent_firm patentscience_firm patentnoscience_firm npatents_cscience prop_cscience patents_cscience_none patents_cscience_any patents_cscience_mixed patents_cscience_only  patentscience_adj_firm patentnoscience_adj_firm npatents_cscience_adj prop_cscience_adj patents_cscience_adj_none patents_cscience_adj_any patents_cscience_adj_mixed patents_cscience_adj_only prop_topq_cscience patents_topq_cscience_any patents_topq_cscience_mixed patents_topq_cscience_none patenttopq_cscience_firm patentnotopq_cscience_firm patents_topq_cscience_conf9_any  patents_topq_cscience_conf9_none patents_topq_cscience_conf9_mix patents_cscience_intext_any patents_cscience_intext_none patents_top4q_cscience_any patents_top4q_cscience_none prop_topq_csciencee_conf9 prop_cscience_intext  prop_top4q_cscience  pat_atonenonus_invt_any pat_allnonusa_invt_any pat_majornonusa_invt_any   pat_atonenonus_invt_any pat_allnonusa_invt_any pat_majornonusa_invt_any  pat_atonenonus_invt_none pat_allnonusa_invt_none patent_largeteam_any patent_largeteam_none patent_largeteam_topq_any patent_largeteam_topq_none pat_majornonusa_invt_none  pat_atonenonus_invt_mix  pat_allnonusa_invt_mix  pat_majornonusa_invt_mix pat_largeteam_mix  pat_largeteam_topq_mix/*npatents_topq_cscience prop_topq_cscience npatents_median_cscience prop_median_cscience npatents_mean_cscience prop_mean_cscience*/


/*********Median Subssample

reghdfe ln_abnretsq_d02 patentaward if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)
reghdfe ln_abnretsq_d02 patents_cscience_adj_none patents_cscience_adj_any if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)


*********top 3 quartile Subssample 
reghdfe ln_abnretsq_d02 patents_topq_cscience_none patents_topq_cscience_any if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)


********All Subssample
reghdfe ln_abnretsq_d02 patents_cscience_none patents_cscience_any if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)


********Intext citation
reghdfe ln_abnretsq_d02 patents_cscience_intext_none patents_cscience_intext_any  if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)


********Conf9 citation
reghdfe ln_abnretsq_d02 patents_topq_cscience_conf9_none patents_topq_cscience_conf9_any  if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)

********TopQ 4
reghdfe ln_abnretsq_d02 patents_top4q_cscience_none patents_top4q_cscience_any  if patent_firm==1 & fyear<2020 , absorb(permno_adj#fyear bdate) cluster(fyear)

*/



tab fyear

reghdfe ln_abnretsq_d02 patentaward if patent_firm==1  & fyear<=2019 , absorb(permno_adj#fyear bdate) cluster(fyear)



keep if e(sample)
bys bdate: gen nfirms = _N
bys bdate patentaward: gen temp = _N
replace temp =. if patentaward == 0
bys bdate: egen nfirms_patent  = max(temp)
drop temp



bys bdate patentaward patents_cscience_any: gen temp = _N
replace temp =. if patents_cscience_any == 0
replace temp =0 if patentaward == 0
bys bdate: egen nfirms_patent_science  = max(temp)
drop temp
gen dow = dow( date)


tab fyear


save "data\analyses.dta", replace