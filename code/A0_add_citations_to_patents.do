********************************************************************************
*
* ADD CITATIONS TO SCIENCE TO PATENTS GRANTED TO DISCERN 2.0 FIRMS (ULTIMATE OWNERS, FOR NOW)
*
********************************************************************************

cd "C:\Users\laris\Dropbox (Duke)\DISCERN Take 2\"

* Preprocess the citation data
import delimited "canonical\Reliance on Science.030824\_pcs_mag_doi_pmid.tsv", clear // 12 vars, 42,822,458 obs

* Keep only patents from the USPTO
keep if uspto == 1 // 10,202,220 observations deleted

* Drop the non-numeric part of the patent number (The first two characters represent the country of the patent office, e.g. US for USPTO.Next is a hyphen (-), followed by the patent numbe)
gen publn_nr = subinstr(patent, "US-", "", 1)

save "data\reliance_on_science_citations.dta", replace


* Combine the patent and citation data
use "Patent match\patents_uo\patents_1980_2021_UO.dta", clear // 13 vars, 1,742,844 observations

unique publn_nr // 1,734,349 the same patent was assigned to two or more DISCERN firms

* Bring in the citations to science; keep the patents in the master that didn't have any citations to science
joinby publn_nr using "data\reliance_on_science_citations.dta", unmatched(master) 
gen cites_science = _merge == 3
drop _merge
unique publn_nr if cites_science // 690,427
unique publn_nr if !cites_science // 1,043,922

save "data\patents_1980_2021_UO_with_citations_to_science.dta", replace

********************************************************************************
* THE END
********************************************************************************