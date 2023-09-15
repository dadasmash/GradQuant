clear all
cap log c
set more off
macro drop _all
estimates clear

cd "C:\Users\Da\Desktop\Stata_DataManagment_July11_23"


import delimited case.csv

gen arrest_date2=date(arrest_date,"YMD")
gen dispos_date2=date(dispos_date,"YMD")

sort person_id  arrest_date2


*gen re_arrest
gen re_arrest=0
local i=1
while `i'<=100{
// choose 100 for demonstrition purpose. should choose number of obs  
replace re_arrest=1 if arrest_date2[_n+1]<dispos_date2[_n]& ///
person_id[_n]==`i'&person_id[_n+1]==`i'

local i=`i'+1
}



gen re_arrest_2=0
bys person_id (arrest_date2) : gen arrest_date3=arrest_date2[_n+1]
bys person_id (arrest_date2) : replace re_arrest_2=1 if arrest_date3< dispos_date2 & arrest_date3!=.

save case_finished,replace 

* Deal with prior_arrests
import delimited C:\Users\Da\Desktop\Stata_DataManagment_July11_23\prior_arrests.csv, clear 

bysort person_id: gen prior_arrest=[_N]
collapse prior_arrest,by(person_id)
save prior_arrests.dta,replace
clear all

use case_finished,clear
merge m:1 person_id using prior_arrests
replace prior_arrest=0 if prior_arrest==.

bysort person_id (arrest_date):replace prior_arrest=prior_arrest+_n-1
drop _merge
save case_finished.dta, replace



**generate age
clear
import delimited demo.csv

encode gender, gen(sex)
encode race, gen(color)

gen bdate2=date(bdate,"YMD")


merge 1:m person_id using case_finished

sort person_id (arrest_date)
gen age = (arrest_date2 - bdate2)/365.25
replace age=floor(age)
save case_finished.dta, replace











