/* Set Directory */

/*Elia  cd "C:\Users\ef122\Duke University\Jay Prakash Nagar - ABF_PatValue_Research"*/
/*Jay*/  cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication" 

********************************************************************************
*******************************Renewal*****************************************
********************************************************************************
clear all
import delimited using "Data\PatentView\Renewal_Data.csv", delimiter(",")

gen publn_nr = real(subinstr(pub_nbr, "US", "", .))

destring publn_nr, replace

duplicates drop publn_nr, force
save "Data\PatentView\Renewal_Data.dta", replace


use "Data\patents_1980_2021_UO_with_citations_to_science.dta", clear
compress
rename (publn_nr) (patent_num)

keep patent_num grant_year patent

gen publn_nr= patent_num

duplicates drop

destring publn_nr, replace

duplicates drop publn_nr, force
merge 1:1 publn_nr using "Data\PatentView\Renewal_Data.dta"

keep if _merge ==3
drop _merge

replace renewed_12th=0 if renewed_12th==.

replace renewed_4th=0 if renewed_4th==.
replace renewed_8th=0 if renewed_8th==.
replace renewed_any=0 if renewed_any==.

sum  renewed_12th renewed_8th renewed_4th renewed_any if grant_year <=2010

sum  renewed_12th renewed_8th renewed_4th renewed_any if (grant_year >=1991) & (grant_year <=2010)

save "Data\PatentView\Renewal_Data_oursample.dta", replace



********************************************************************************
*******************************Trilateral_pat*****************************************
********************************************************************************
clear all
import delimited using "Data\PatentView\Trilateral_pat.csv", delimiter(",")

gen publn_nr = real(subinstr(pub_nbr, "US", "", .))

destring publn_nr, replace

duplicates drop publn_nr, force
save "Data\PatentView\Trilateral_pat.dta", replace


use "Data\patents_1980_2021_UO_with_citations_to_science.dta", clear
compress
rename (publn_nr) (patent_num)

keep patent_num grant_year patent

gen publn_nr= patent_num

duplicates drop

destring publn_nr, replace

duplicates drop publn_nr, force
merge 1:1 publn_nr using "Data\PatentView\Trilateral_pat.dta"

keep if _merge ==3
drop _merge

replace trilateral_pat=0 if trilateral_pat==.


sum  trilateral_pat if grant_year <=2010

sum  trilateral_pat if (grant_year >=1991) & (grant_year <=2010)

save "Data\PatentView\Trilateral_pat_oursample.dta", replace



********************************************************************************
**************************Patent Reassignment***********************************
********************************************************************************

use "Data\PatentView\assignment\assignment_conveyance.dta", clear

drop if employer_assign ==1


keep if convey_ty=="assignment"|convey_ty=="merger"|convey_ty=="govern"

*keep if convey_ty=="assignment"|convey_ty=="merger"

merge 1:m rf_id using "Data\PatentView\assignment\documentid_admin.dta"

keep if _merge==3
drop _merge



sort rf_id
quietly by rf_id:  gen dup = cond(_N==1,0,_n)

drop if dup>0


/*
 convey_ty |      Freq.     Percent        Cum.
------------+-----------------------------------
 assignment |    960,673       86.53       86.53
     govern |    104,310        9.40       95.92
     merger |     45,268        4.08      100.00
------------+-----------------------------------
      Total |  1,110,251      100.00
*/

rename admin_pat_no_for_appno patent_num

gen reassign_dummy=1

bysort patent_num: egen trade_count = count(rf_id)



keep patent_num reassign_dummy trade_count

duplicates drop

save "Data\patent_reassignment.dta", replace


**************************Litigation Data**********************



/*Jay*/  cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication" 

clear all
**************************Litigation Data**********************
use "Data\patents_litigation.dta", clear

rename patent patent_str

gen patent_num = patent_str

drop if patent_num==""
bys patent_num: keep if _n == 1

gen litigation = 1
label var litigation "Litigation ==1"

keep patent_num  litigation


* convert numeric patent_num -> string (no commas/decimals)
tostring patent_num, replace format(%10.0f)

* optional: trim spaces (good hygiene)
replace patent_num = strtrim(patent_num)




save "Data\patents_litigation_dummy.dta", replace