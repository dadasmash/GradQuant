*** Data Cleaning and Management with Stata
*** Da Gong
*** GradQuant


** Ref: Chuck Huber
** https://blog.stata.com/2017/11/15/data-management-made-easy/


set more off 
clear all


****
*How to append files into a single dataset
use "https://www.stata-press.com/data/r18/even", clear 
append using "https://www.stata-press.com/data/r18/odd"



*How to merge files into a single dataset
webuse autoexpense, clear 

use "C:\Users\Da\Desktop\Stata_DataManagment_July11_23\autoexpense.dta", clear 

replace mpg=mpg*1.5
list 

webuse autosize, clear 

cd "C:\Users\Da\Desktop\Stata_DataManagment_July11_23"

use autosize, clear 
list in 1/3

merge 1:1 make using autosize

merge 1:1 make using "C:\Users\Da\Desktop\Stata_DataManagment_July11_23\autosize.dta"
* make is the key variable 


*** Merge combine with tempfile
use autoexpense, clear 
replace mpg=mpg*1.5
tempfile data1
save "`data1'"



use autosize, clear 
gen size=weight*length
tempfile data2
save "`data2'"
save autosize2, replace 

use autoF, clear
merge 1:1 make using `data1'
* local macro 

cap drop _merge
merge 1:1 make using `data2'
cap drop _merge


*label variables
label var price "Price of Cars"

gen income_023= weight-length
label var income_023 "income adjusted by CPI"

histogram price

*graph bar price, over(e001_234, label(angle(90) labsize(tiny))) 
 
 
 
* label the values of categorical variables
use auto, clear 
tab foreign

sum price, d
return list 


gen PQ =1 if price < r(p25)
replace PQ=2 if price >=r(p25) & price <r(p50)
replace PQ=3 if price >=r(p50) & price <r(p75)
replace PQ=4 if price >=r(p75) & price < . 

label var PQ "Price Quantile"

label define PQ_v 1 "Q1" 2 "Q2" 3 "Q3" 4 "Quantile 4"
label values PQ PQ_v

tab PQ


label list PQ_v
tab PQ

***convert a string variable to a numeric variable
sum price

tostring price, gen(price2)
sum price 
sum price2
drop price 
destring price2, gen(price) 


replace price2="474s" in 2
replace price2="4o99" if _n==1

sum price2
drop price 


destring price2, gen(price) force
sum price 

***convert categorical string variables to labeled numeric variables
encode make, gen(make_n)

*** Bysort 
sort price 

sort foreign price


bysort PQ (trunk): gen count=_N
bys PQ trunk: gen count2 =_N
bysort PQ (trunk): gen order=_n

bysort PQ (trunk) : keep if order==_N


** Check Duplicated identifier
use auto, clear 
gen id= _n
replace id=5 in 66

bys id : gen count=_N

list id if count>1 
bys id: keep if _n==1


***Creat a new variable
use auto, clear 
gen bmi3 = weight / ((length/100)^2)

sum weight
return list 
gen z_score = (weight - r(mean) ) / r(sd)


**********
*** Matrix
***********
 
use wage1.dta, clear
*Whether There Is Gender Pay Gap? 
twoway (hist wage if female==0,color(red%30)) ///
(hist wage if female==1,color(green%30)), ///
legend(order(1 "Male" 2 "Female") col(1))

twoway (hist wage if female==0,color(blue)) ///
(hist wage if female==1, fcolor(none) lcolor(black)), ///
legend(order(1 "Male" 2 "Female") col(1))
*https://stats.idre.ucla.edu/stata/faq/how-can-i-overlay-two-histograms/

ttest wage , by (female)

reg wage female

*take log to wage, we get lwage
twoway (hist lwage if female==0,color(blue)) ///
(hist lwage if female==1, fcolor(none) lcolor(black)), ///
legend(order(1 "Male" 2 "Female") col(1))

reg lwage female
*on average, women make around 60cents for every dollar that men make

bys female: sum educ
bysort female: sum exper

twoway scatter lwage educ
twoway (scatter lwage exper) (qfit lwage exper)

reg lwage female educ exper expersq
test (educ=exper) 
test educ exper expersq 

*Now the gender pay gap reduces to about 34 cents. 

twoway (lfit lwage educ if female==0) ///
(lfit lwage educ if female==1), ///
legend(order(1 "Male" 2 "Female") col(2))

*But, are the slopes  same for women and men?


*graph coefficient of education contitional on different experience level

*https://blog.uvm.edu/tbplante/2019/10/30/working-with-stata-regression-results-matrix-matrices-macros-oh-my/
*https://www.stata.com/meeting/germany14/abstracts/materials/de14_jann.pdf
*https://graphworkflow.com/implantations/recast/
gen exper_lv=1 if exper<=10 
replace exper_lv=2 if exper>10& exper<=20
replace exper_lv=3 if exper>20& exper<=30
replace exper_lv=4 if exper>30

matrix A=J(4,6,.)
matrix coln A=edu_f edu_f_ll95 edu_f_ul95 edu_m edu_m_ll95 edu_m_ul95
matrix rown A=1 2 3 4 

forvalue i=1/4{
quiet reg lwage edu if exper_lv==`i' & female==1
matrix B=r(table)
matrix A[`i',1]=B[1,1]
matrix A[`i',2]=B[5,1]
matrix A[`i',3] =B[6,1]
}

forvalue i=1/4{
quiet reg lwage edu if exper_lv==`i' & female==0
matrix B=r(table)
matrix A[`i',4]=B[1,1]
matrix A[`i',5]=B[5,1]
matrix A[`i',6] =B[6,1]
}

coefplot (matrix(A[,1]),ci((A[,2] A[,3])))(matrix(A[,4]), ci((A[,5] A[,6]))), ///
ytitle(coefficient of educ)  yline(0) vertic legend(order(1 "Female" 3 "male")) ///
recast(connected) xtitle(experience level)  ciopts(recast(rcap)) citop

*you can use this trick in the parallel trends of DID identification in graduate study 

**Another approach by twoway which is better
*https://github.com/worldbank/Stata-IE-Visual-Library/tree/master/Library/Regression%20coefficients/Comparison%20of%20marginal%20effects%20from%20different%20models


***Example 

