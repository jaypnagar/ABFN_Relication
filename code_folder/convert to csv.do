set more off
clear

 
cd "C:\Users\jayho\Downloads\Ashish Work\119043-V1"


use "intermediate_data\MergedPatentData.dta"

export delimited using "intermediate_data\MergedPatentData.csv", replace

clear all 

use "intermediate_data\MergedPatentData_v2.dta"

export delimited using "intermediate_data\MergedPatentData_v2.csv", replace

clear all 

use "intermediate_data\Breakthrough_Patents.dta"

export delimited using "intermediate_data\Breakthrough_Patents.csv", replace