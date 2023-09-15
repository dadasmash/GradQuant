


**********************************
***** Muiltiple treatment time point
***********************************
clear all
set obs 60
set seed 10101
gen id =_n
/// 
expand 11
drop in 1/60
count
///
bys id: gen time = _n+1999
xtset id time
///
gen x1 = rnormal(1,7)
gen x2 = rnormal(2,5)
sort time id
by time: gen ind = _n
sort id time
by id: gen T = _n
gen y = 0
///id 1-20 treated in 2004 ，21-40 treated in 2006 ，41-60 never treated
gen D = 0
gen birth_date = 0
gen G = 0
forvalues i = 1/20{
replace D = 1 if id == `i' & time >= 2004
replace birth_date = 2004 if id == `i'
replace G = 1 if id == `i'
}

forvalues i = 21/40{
replace D = 1 if id == `i' & time >= 2006
replace birth_date = 2006 if id == `i'
replace G = 2 if id == `i'
}



bysort id: gen y0 = 10 + 5 * x1 + 3 * x2 + T + ind + rnormal()
bysort id: gen y1 = 10 + 5 * x1 + 3 * x2 + T + ind + (time - birth_date + 1) * 3 +rnormal() if time >= 2004 & id >= 1 & id <= 20
bysort id: replace y1 = 10 + 5 * x1 + 3 * x2 + T + ind + (time - birth_date + 1) * 3 +rnormal() if time >= 2006 & id >= 21 & id <= 40
bysort id: replace y1 = 10 + 5 * x1 + 3 * x2 + T + ind + rnormal() if y1 == .
replace y = y0 + D * (y1 - y0)



xtreg y x1 x2 , fe r
predict e, ue
binscatter e time, line(connect) by(G)



reghdfe y D x1 x2, absorb(id time) vce(robust)


********

gen event = time - birth_date if id <= 40
replace event = -4 if event <= -4
tab event, gen(eventt)
forvalues i = 1/10{
replace eventt`i' = 0 if eventt`i' == .
}

replace eventt4=0

reghdfe y eventt* x1 x2, absorb(id time) vce(cluster id)
eststo bb

set scheme s2color
coefplot (bb, label("Pm2.5") msymbol(d)  mcolor(pink)  ciopts(recast(rcap) color(pink)) lc(pink)),   ///
keep(eventt*) vertical ytitle(Coefficient of Interest) ylabel(-.3(.05)0.1,angle(360))  xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "0" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5")  xtitle("Number of months since lockdown") ///
 levels(95)  yline(0,lcolor(gs7%50) lp(dash)) xline(4,lcolor(gs7%50) lp(dash)) ///
omitted baselevels ///
     groups(d_portion535-d_portion564 = `"{bf:Pre-Treated}"'        ///
             d_portion565-d_portion595= `""{bf:Post-Treated}" "' )  ///
			title("Outcome: log Nightlight")   legend(order(2 "DD")) ///
graphregion(color(white)) bgcolor(white)  



**Sun-Abraham


gen  never_treat = (id>40)
gen treat_date=2004 if id>=1&id<=20
gen treat_date=2006 if id>=21&id<=40

eventstudyinteract y eventt*, cohort(treat_date) control_cohort(never_treat) absorb(i.id i.time) vce(cluster id) covariates(x1 x2)
matrix A = r(table)


***
set scheme s2color
coefplot (bb, label("Pm2.5") msymbol(d)  mcolor(pink)  ciopts(recast(rcap) color(pink)) lc(pink))  (matrix(A[1,]),ci((A[5,] A[6,]))  mcolor(orange)  ciopts(recast(rcap) color(orange)) lc(orange)),   ///
keep(eventt*) vertical ytitle(Coefficient of Interest) ylabel(-.3(.05)0.1,angle(360))  xlabel(1 "-4" 2 "-3" 3 "-2" 4 "-1" 5 "0" 6 "1" 7 "2" 8 "3" 9 "4" 10 "5")   xtitle("Number of months since lockdown") ///
 levels(95)  yline(0,lcolor(gs7%50) lp(dash)) xline(4,lcolor(gs7%50) lp(dash)) ///
omitted baselevels ///
     groups(d_portion535-d_portion564 = `"{bf:Pre-Treated}"'        ///
             d_portion565-d_portion595= `""{bf:Post-Treated}" "' )  ///
			title("Outcome: log Nightlight")   legend(order(2 "DD" 4 "Sun-Abraham")) ///
graphregion(color(white)) bgcolor(white)  



*******
* https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
g time_to_treat = time - birth_date
replace time_to_treat = -4 if time_to_treat  <= -4
replace time_to_treat = 0 if id>40
*if missing(_nfd) // by this step, the never treated is put into the control group. so we don't need to include interaction term in the last step regression. 
* this will determine the difference
* btw controls and treated states


* Stata won't allow factors with negative values, so let's shift
* time-to-treat to start at 0, keeping track of where the true -1 is
summ time_to_treat
g shifted_ttt = time_to_treat - r(min)
summ shifted_ttt if time_to_treat == -1
// local true_neg1 = r(mean)
// reghdfe asmrs ib`true_neg1'.shifted_ttt pcinc asmrh cases, a(stfips year) vce(cluster stfips)
* Regress on our interaction terms with FEs for group and year,
* clustering at the group (state) level
* use ib# to specify our reference group


** the original code has a mistake??? should include interaction term i think
reghdfe y ib3.shifted_ttt x1 x2, a(id time) vce(cluster id)
