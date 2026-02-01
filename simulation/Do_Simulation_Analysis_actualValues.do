cd "C:\Users\jayho\OneDrive - Duke University\ABF_Replication\code\simulation_diff_check"

use "simulate_data_highpatdays.dta", clear

* Year index
gen year = int(day/365)
tab year
drop if year==40

*******************Two Distribution********************************************
gen double r = e + x
gen double abnretsq = r^2
bysort firm_id year: egen double measured_vol = mean(abnretsq)

* Dummies
gen byte no_science = (science_dummy==0 & patent_dummy==1)

* Safe log of squared returns (avoid log(0))
gen double r_2 = r^2
gen double log_r_2 = log(r_2 + 1e-12)

label var patent_dummy  "Patent Dummy"
label var science_dummy "Science Patent Dummy"
label var no_science    "No Science Patent Dummy"

******** Regression 1: Any patent day
reghdfe log_r_2 patent_dummy, absorb(firm_id#year day) cluster(year)

gen double gamma_joint = _b[patent_dummy]
gen double delta_joint = 1 - exp(-gamma_joint)

bysort firm_id year: gen int tot_days = _N
bysort firm_id year: egen int pat_days = total(patent_dummy)
gen double pat_days_ratio = pat_days / tot_days

gen double vol_e_joint = measured_vol * (1 + pat_days_ratio*(exp(gamma_joint)-1))^(-1)

gen double sqrt_delta = sqrt(delta_joint)
gen double functions_input = - sqrt_delta * (r / sqrt(vol_e_joint))
gen double numerator = normalden(functions_input)
gen double denominator = 1 - normal(functions_input)
gen double exp_cond_value = delta_joint*r + sqrt_delta*sqrt(vol_e_joint)*(numerator/denominator)

gen double pat_value_kpss = exp_cond_value


******** Regression 2: Science vs No-science patent days
reghdfe log_r_2 science_dummy no_science, absorb(firm_id#year day) cluster(year)
test _b[science_dummy] = _b[no_science]

gen double gamma_noscience  = _b[no_science]
gen double gamma_anyscience = _b[science_dummy]

gen double delta_noscience  = 1 - exp(-gamma_noscience)
gen double delta_anyscience = 1 - exp(-gamma_anyscience)

bysort firm_id year: egen int pat_days_nosci  = total(no_science)
bysort firm_id year: egen int pat_days_anysci = total(science_dummy)

gen double pat_days_ratio_nosci  = pat_days_nosci  / tot_days
gen double pat_days_ratio_anysci = pat_days_anysci / tot_days

gen double vol_e_new = measured_vol * (1 ///
    + pat_days_ratio_nosci *(exp(gamma_noscience)-1) ///
    + pat_days_ratio_anysci*(exp(gamma_anyscience)-1) ///
    )^(-1)

* Old-style conditional expectations (using delta = 1-exp(-gamma))
gen double sqrt_delta_noscience = sqrt(delta_noscience)
gen double z_noscience = - sqrt_delta_noscience * (r / sqrt(vol_e_new))
gen double exp_cond_value_noscience = delta_noscience*r ///
    + sqrt_delta_noscience*sqrt(vol_e_new)*(normalden(z_noscience)/(1-normal(z_noscience)))

gen double sqrt_delta_anyscience = sqrt(delta_anyscience)
gen double z_anyscience = - sqrt_delta_anyscience * (r / sqrt(vol_e_new))
gen double exp_cond_value_anyscience = delta_anyscience*r ///
    + sqrt_delta_anyscience*sqrt(vol_e_new)*(normalden(z_anyscience)/(1-normal(z_anyscience)))



gen 	pat_value_kpss_new = ((exp_cond_value_noscience)) if no_science == 1

replace pat_value_kpss_new = ((exp_cond_value_anyscience ))  if science_dummy == 1
	
	
************************************************************************


gen double delta_anyscience_eq = 0.0473
gen double delta_noscience_eq  = 0.03922



gen vol_e_new_eq =  noise_variance^2


* Plug-in conditional expectations using delta_eq
gen double sqrt_delta_noscience_eq = sqrt(delta_noscience_eq)
gen double z_noscience_eq = - sqrt_delta_noscience_eq * (r / sqrt(vol_e_new_eq))
gen double exp_cond_value_noscience_eq = delta_noscience_eq*r ///
    + sqrt_delta_noscience_eq*sqrt(vol_e_new_eq)*(normalden(z_noscience_eq)/(1-normal(z_noscience_eq)))

gen double sqrt_delta_anyscience_eq = sqrt(delta_anyscience_eq)
gen double z_anyscience_eq = - sqrt_delta_anyscience_eq * (r / sqrt(vol_e_new_eq))
gen double exp_cond_value_anyscience_eq = delta_anyscience_eq*r ///
    + sqrt_delta_anyscience_eq*sqrt(vol_e_new_eq)*(normalden(z_anyscience_eq)/(1-normal(z_anyscience_eq)))

label var exp_cond_value_noscience_eq "E[x|r] no-science (delta eq)"
label var exp_cond_value_anyscience_eq "E[x|r] science (delta eq)"

* Build a single KPSS-like value series (only defined on patent days)
gen double pat_value_kpss_new_eq = .
replace pat_value_kpss_new_eq = exp_cond_value_noscience_eq  if no_science==1
replace pat_value_kpss_new_eq = exp_cond_value_anyscience_eq if science_dummy==1

************************************************************************
* Regressions / validation


keep if patent_dummy==1


est clear



eststo: reghdfe x science_dummy,absorb(firm_id year) vce(robust)
estadd local Years = "Yes"
estadd local Firm = "Yes"
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)

eststo: reghdfe pat_value_kpss science_dummy,absorb(firm_id year) vce(robust)
estadd local Years = "Yes"
estadd local Firm = "Yes"
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)



eststo: reghdfe pat_value_kpss_new science_dummy,absorb(firm_id year) vce(robust)
estadd local Years = "Yes"
estadd local Firm = "Yes"
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)


eststo: reghdfe pat_value_kpss_new_eq science_dummy,absorb(firm_id year) vce(robust)
estadd local Years = "Yes"
estadd local Firm = "Yes"
qui sum `e(depvar)' if e(sample)
estadd scalar mean_dv = r(mean)

esttab using "Simulation_ScienceNoScience_test_withacutal.tex", replace cells(b(star fmt(%12.3f)) se(par)) ///
	stats( Years Firm r2 N mean_dv, ///
	label(  "Year Fixed Effects" "Firm Fixed Effects" "R^2" "N" "Mean DV") ///
	 fmt( %9.0gc %9.0gc  %9.3f %12.0gc %9.3f)) /// 
	starlevels(* .10 ** .05 *** .01) label collabels(none) ///
	nomtitle  ///
	mgroups("Simulated Data" "One distribution" "Two distribution" "Using Actual SNR and Variance", pattern(1 1 1 1  ) ///
       prefix(\multicolumn{@span}{c}{) suffix(}) ///
      span erepeat(\cmidrule(lr){@span})) ///
      order(science_dummy )