/* Set Directory */

/*Elia  cd "C:\Users\ef122\Duke University\Jay Prakash Nagar - ABF_PatValue_Research"*/
/*Jay cd "C:\Users\jayho\Downloads\simulatation"*/ 

cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\Code\simulatation" 

use "simulate_data_highpatdays.dta",clear 



gen year = int(abs(day/365))
tab year
drop if year==40




********************For single Distribution***********************************
gen r_s = e+x_single_dist
gen abnretsq = r_s *r_s
bysort firm_id year: egen measured_vol = mean(abnretsq)

gen r_s_2 = r_s*r_s

gen log_r_s_2 = log(r_s_2) 




reghdfe log_r_s_2  patent_dummy,absorb(firm_id#year day) cluster(year)

/* estimate variance of measurement error */
gen gamma_joint = _b[patent_dummy]
display gamma_joint
gen delta_joint = 1-exp(-gamma_joint)
tab delta_joint

bysort firm_id year : gen tot_days = _N
bysort firm_id year: egen pat_days = total(patent_dummy)
gen pat_days_ratio = pat_days / tot_days
gen vol_e_joint =  measured_vol * ((1 + pat_days_ratio* (exp(gamma_joint) - 1))^-1)




gen sqrt_delta = sqrt(delta_joint)
tab sqrt_delta
gen functions_input = (-1) * (sqrt_delta) * (r_s / sqrt(vol_e_joint))
gen numerator = normalden(functions_input)
gen denominator = 1 - normal(functions_input)
gen exp_cond_value = delta_joint * r_s + sqrt_delta * sqrt(vol_e_joint) * (numerator / denominator)


*gen pi = 0.56 // this is the unconditional probability of success
gen pat_value_kpss_single = ((exp_cond_value))

corr pat_value_kpss_single x_single_dist if patent_dummy==1

drop gamma_joint delta_joint tot_days pat_days pat_days_ratio vol_e_joint sqrt_delta functions_input numerator denominator exp_cond_value abnretsq measured_vol

*******************Two Distribution********************************************
gen r = e+x
gen abnretsq = r * r
bysort firm_id year: egen measured_vol = mean(abnretsq)

sum r
sum e if science_dummy==1
sum x if science_dummy==1 & patent_dummy==1
sum x if science_dummy==0 & patent_dummy==1

gen no_science =1 if science_dummy==0 & patent_dummy==1
replace no_science=0 if no_science==.
sum x



gen r_2 = r*r

gen log_r_2 = log(r_2) 



label var patent_dummy "Patent Dummy"

label var science_dummy  "Science Patent Dummy"

label var no_science "No Science Patent Dummy"

********
reghdfe log_r_2  patent_dummy,absorb(firm_id#year day) cluster(year)

/* estimate variance of measurement error */
gen gamma_joint = _b[patent_dummy]
display gamma_joint
gen delta_joint = 1-exp(-gamma_joint)
tab delta_joint

bysort firm_id year : gen tot_days = _N
bysort firm_id year: egen pat_days = total(patent_dummy)
gen pat_days_ratio = pat_days / tot_days
gen vol_e_joint =  measured_vol * ((1 + pat_days_ratio* (exp(gamma_joint) - 1))^-1)




gen sqrt_delta = sqrt(delta_joint)
tab sqrt_delta
gen functions_input = (-1) * (sqrt_delta) * (r / sqrt(vol_e_joint))
gen numerator = normalden(functions_input)
gen denominator = 1 - normal(functions_input)
gen exp_cond_value = delta_joint * r + sqrt_delta * sqrt(vol_e_joint) * (numerator / denominator)


*gen pi = 0.56 // this is the unconditional probability of success
gen pat_value_kpss = ((exp_cond_value))
********

reghdfe log_r_2  science_dummy no_science,absorb(firm_id#year day) cluster(year)

test _b[science_dummy] =_b[no_science]

gen gamma_noscience = _b[no_science]
display gamma_noscience
gen delta_noscience = 1 - exp(-gamma_noscience)
display delta_noscience

gen gamma_anyscience = _b[science_dummy]
display gamma_anyscience
gen delta_anyscience = 1 - exp(-gamma_anyscience)
display delta_anyscience






bysort firm_id year: egen pat_days_nosci = total(no_science)
gen pat_days_ratio_nosci = pat_days_nosci / tot_days


bysort firm_id year: egen pat_days_anysci = total(science_dummy)
gen pat_days_ratio_anysci = pat_days_anysci / tot_days




gen vol_e_new =  measured_vol * ((1 + ( pat_days_ratio_nosci * (exp(gamma_noscience) - 1)) + (pat_days_ratio_anysci * (exp(gamma_anyscience) - 1)))^-1)



gen sqrt_delta_noscience = sqrt(delta_noscience)
gen functions_input_noscience = (-1) * (sqrt_delta_noscience) * (r / sqrt(vol_e_new))
gen numerator_noscience = normalden(functions_input_noscience)
gen denominator_noscience = 1 - normal(functions_input_noscience)
gen exp_cond_value_noscience = delta_noscience * r + sqrt_delta_noscience * sqrt(vol_e_new) * (numerator_noscience / denominator_noscience)



gen sqrt_delta_anyscience = sqrt(delta_anyscience)
gen functions_input_anyscience = (-1) * (sqrt_delta_anyscience) * (r / sqrt(vol_e_new))
gen numerator_anyscience = normalden(functions_input_anyscience)
gen denominator_anyscience = 1 - normal(functions_input_anyscience)
gen exp_cond_value_anyscience = delta_anyscience * r + sqrt_delta_anyscience * sqrt(vol_e_new) * (numerator_anyscience / denominator_anyscience)






est clear
* 1. OLS
* start with no controls, add quality controls


eststo:reghdfe log_r_2  patent_dummy,absorb(firm_id#year day) cluster(year)
estadd local Years = "Yes"
estadd local retun_day = "Yes"

eststo:reghdfe log_r_2  science_dummy no_science,absorb(firm_id#year day) cluster(year)
estadd local Years = "Yes"
estadd local retun_day = "Yes"


esttab using "Simulation_ScienceNoScience_gamma_v2.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats( Years retun_day r2 N, ///
	label(  "Firm*Year Fixed Effects" "Return Day" "R^2" "N") ///
	 fmt( %9.0gc %9.0gc  %9.3f %12.0gc)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("KPSS: One Distribution" "Modified: Two distribution", pattern(1 1 0 ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(science_dummy )


	  
reghdfe log_r_2  science_dummy no_science,absorb(firm_id#year day) cluster(year)	  



gen pi = 0.56

gen 	pat_value_kpss_new = ((exp_cond_value_noscience)) if no_science == 1

replace pat_value_kpss_new = ((exp_cond_value_anyscience ))  if science_dummy == 1


corr x pat_value_kpss pat_value_kpss_new

keep if patent_dummy==1


est clear

eststo: reghdfe x science_dummy,absorb( year) vce(robust)
estadd local Years = "Yes"
estadd local Firm = "No"
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)

eststo: reghdfe x science_dummy,absorb(firm_id year) vce(robust)
estadd local Years = "Yes"
estadd local Firm = "Yes"
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)

eststo: reghdfe pat_value_kpss science_dummy,absorb( year) vce(robust)
estadd local Years = "Yes"
estadd local Firm = "No"
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)

eststo: reghdfe pat_value_kpss science_dummy,absorb(firm_id year) vce(robust)
estadd local Years = "Yes"
estadd local Firm = "Yes"
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)

eststo: reghdfe pat_value_kpss_new science_dummy,absorb( year) vce(robust)
estadd local Years = "Yes"
estadd local Firm = "No"
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)

eststo: reghdfe pat_value_kpss_new science_dummy,absorb(firm_id year) vce(robust)
estadd local Years = "Yes"
estadd local Firm = "Yes"
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)


esttab using "Simulation_ScienceNoScience_test_v2.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats( Years Firm r2 N mean_dv, ///
	label(  "Year Fixed Effects" "Firm Fixed Effects" "R^2" "N" "Mean DV") ///
	 fmt( %9.0gc %9.0gc  %9.3f %12.0gc %9.3f)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Simulated Data" "One distribution" "Two distribution", pattern(1 0 1 0 1 0  ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(science_dummy )
	  
	  
label var pat_value_kpss_new "Estimated Value (two distribution)"
label var pat_value_kpss "Estimated Value (One distribution)"

	  
corr x pat_value_kpss pat_value_kpss_new

est clear
eststo sumstats: qui estpost sum x pat_value_kpss pat_value_kpss_new, d
esttab sumstats using "sum_simulation_v2.tex", cells("count(fmt(0)) mean(fmt(3)) sd( fmt(3)) p10(fmt(3)) p50(fmt(3))  p90(fmt(3))") nonumber nomtitle noobs label collabels("Obs." "Mean" "Std. Dev." "10\%" "50\%" "90\%") replace	  



***********************correlation table**********************************************
local vars  x pat_value_kpss pat_value_kpss_new 
*Basic command:
corr `vars'
*More complicated
estpost correlate `vars',matrix 
esttab . using "corr_simulation_v2.tex", unstack b(3)  noobs nostar  nonumber replace label
********************************************************

gen diff =  x - pat_value_kpss


gen diff_new =  x - pat_value_kpss_new

histogram diff

histogram diff_new

sum diff diff_new