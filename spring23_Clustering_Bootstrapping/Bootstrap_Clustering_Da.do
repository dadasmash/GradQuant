*************
*Bootstrapping and Randomization Inference with Stata
*GradQuant
*Da Gong  dgong005@ucr.edu
************

*** 
*Part of the material is adopted from Dr. Joseph R. Cummins class material and also 
* inspired by slides from  Dr. Jessamyn Schaller
* An example is adopted from Dr.Francis Smart
* 
***


*****************************
**Statistical Inference
******************************

set more off
clear all

set ob 500
gen ob = _n
gen T = ob <=250

gen eps = rnormal()

gen Y = 2*T + eps

regress Y T 
ereturn list

*Manually calculate beta_hat (T)
matrix define X=J(_N, 1, .)

quiet sum T
scalar T_mean=r(mean)
forvalue i = 1/500{
matrix X[`i',1]=T[`i']-T_mean
}


matrix define Y=J(_N, 1, .)
quiet sum Y
scalar Y_mean=r(mean)
forvalue i = 1/500{
matrix Y[`i',1]=Y[`i']-Y_mean
}

scalar covn=0
local i=1
while `i'<= 500{
scalar covn=Y[`i',1]*X[`i',1]+covn
local i= `i'+1
}


scalar x_vn=0
local i=1
while `i'<= 500{
scalar x_vn=X[`i',1]*X[`i',1]+x_vn
local i= `i'+1
}


scalar beta_hat=covn/x_vn
dis beta_hat

**S.D. of beta_hat (T) 
*https://lukesonnet.com/teaching/inference/200d_standard_errors.pdf
qui regress Y T 
predict resid, residuals

mkmat resid, matrix(R)
*matrix list R

scalar e_vn=0
local i=1
while `i'<= 500{
scalar e_vn=R[`i',1]*R[`i',1]+e_vn
local i= `i'+1
}

scalar sigma2_hat=  e_vn/ (500-2)
scalar se=  sqrt(sigma2_hat/x_vn)
dis se

**you may want to review law of large number and central limit theorem in the statistics class
set more off
clear all

cap program drop ols

program define ols, rclass
drop _all
set ob 500
gen ob = _n
gen T = ob <=250
gen eps = rnormal()
gen Y = 2*T + eps
reg Y T
end

simulate _b, reps(1000) nodots : ols
* command bootstrap is very similar 
sum

hist _b_T, fcolor(none)  lcolor(black) lpattern(dash) xline(2)

**what is beta_hat
** what is the distribution of beta_hat (sd)
** what is the S.E (or S.D.) of beta from one single regression

** how the S.D. of beta_hat different from the S.D. we get from a single ols regression? 


**We dont know the sigma2 from the population!! we have to use sigma2_hat, estimated from the sample as substitution
* e.g. Mean: https://online.stat.psu.edu/stat415/lesson/10/10.2

*************************
*Another Example
**************************
clear all
set mem 1g
set more off
*** Sample of Simple, OLS Bootstrap


*For Saving Data
tempfile bsfile cleandata

*set ob 1
*gen hold = 1
*save `bsfile'

clear
set ob 600

*Generate treatment groups
gen ob = _n
gen T = ob<=300


gen X = rnormal()
gen eps = rnormal()
gen Y = 3*X + 2*T + eps

save `cleandata', replace

reg Y T X
gen BetaHat = _b[T]
contract BetaHat
gen REAL = 1
save `bsfile', replace

** The best way to get se of beta_hat is to run the experienment in real world for e.g. 1000 time. But...


*************************
*Bootstraping
**************************

** bootstrapping (similar with we run a experienment over and over again)
** draw subsample with replacement from the sample you have. treat the sample you have as "population" (with the assumption that your sample is representative)


** Repeat Estimating BetaHat using Bootstrap Samples of the Oriignal Data
	*NOTE: NOT DRAWING NEW DATA FROM DGP, DRAWING WITH REPLACEMENT FROM ORIGINAL DATA
	
**The process for a bootstrap is (ref:https://theeffectbook.net/ch-StatisticalAdjustment.html#your-standard-errors-are-probably-wrong):
*1. Start with a data set with N observations.
*2. Randomly sample N observations from that data set, allowing the same observation to be drawn more than once. This is a "bootstrap sample".
*3. Estimate our statistic of interest using the bootstrap sample.
* 4. Repeat steps 1-3 a whole bunch of times.
* 5 Look at the distribution of estimates from across all the times we repeated steps 1-3. The standard deviation of that distribution is the standard error of the estimate. The 2.5th and 97.5th percentiles show the 95% confidence interval, and so on.

* i could be  5000 and so on...
forvalues i= 1/100 {
clear all
use `cleandata'


bsample

reg Y T X

gen BetaHat = _b[T]

contract BetaHat
gen REAL = 0

append using `bsfile'
save `bsfile', replace

}

use `bsfile', clear

sum BetaHat if REAL==0
	gen BSSE = r(sd)

twoway (kdensity BetaHat if REAL==0)


*********************
*Cluster
********************

clear
set more off
estimates clear
set seed 123
set obs 20

*Generate group-level X-variables and error terms
gen X = rnormal(0, 1)
gen eps = rnormal(0, 1)

gen epsgroup = _n
gen T = epsgroup<= (_N/2)

*More than one ob per cluster

*Make 30 observations per group
expand 30
bysort epsgroup: gen groupob = _n

*Generate deviations in X and Epsilon by period-by-observation (deviations from group levels above)
*gen x_within = 0
* https://cameron.econ.ucdavis.edu/research/Cameron_Miller_JHR_2015_February.pdf
* page 4
gen x_within = rnormal(0, 0.1)
gen newX = X + x_within


gen eps_sd= x_within
bys epsgroup: replace eps_sd=eps_sd[1]
gen eps_within = rnormal(0, abs(eps_sd)*3)

*??????
*gen eps_within = rnormal(0, abs(x_within*3))
*gen eps_within = rnormal(0, x_within*3)
*??????


*gen eps_within = 0
*Generate the outcome variable
gen Y2 = 3*newX + 2*T + eps_within + eps

* eps_within is a cluster-speciffic error or common shock that is i.i.d.
* eps is an idiosyncratic error that is i.i.d.

twoway (scatter Y2 newX if T==0, msize(small) mc(gray)) || ///
	(scatter Y2 newX if epsgroup==1, msize(small) mc(red)) || ///
	(lfit Y2 newX, lc(black) lw(thick)), name("`h'") ///
	title("Best Fit Regression Line with Treatment Effect") legend(order( 1 "Control" 2 "Treatment")) 
* ref: https://theeffectbook.net/ch-StatisticalAdjustment.html#your-standard-errors-are-probably-wrong
* they’re likely to all be similarly above-prediction or below-prediction together because of the similar environments they face - their errors will be correlated with each other.	

*** Loop 	
clear all 
global list "1 .5 .1 .01"
global list2 "a b c d"
local len_list : word count $list
di `len_list'

forvalue i = 1(1)`len_list'{
local j :word `i' of $list
local h :word `i' of $list2
dis `j'
dis "`h'"


clear
set more off
estimates clear
set seed 123
set obs 20

*Generate group-level X-variables and error terms
gen X = rnormal(0, 1)
gen eps = rnormal(0, 1)

gen epsgroup = _n
gen T = epsgroup<= (_N/2)

*More than one ob per cluster

*Make 30 observations per group
expand 30
bysort epsgroup: gen groupob = _n

*Generate deviations in X and Epsilon by period-by-observation (deviations from group levels above)
*gen x_within = 0
* https://cameron.econ.ucdavis.edu/research/Cameron_Miller_JHR_2015_February.pdf
* page 4
gen x_within = rnormal(0, `j')
gen newX = X + x_within


gen eps_sd= x_within
bys epsgroup: replace eps_sd=eps_sd[1]
gen eps_within = rnormal(0, abs(eps_sd)*3)

*??????
*gen eps_within = rnormal(0, abs(x_within*3))
*gen eps_within = rnormal(0, x_within*3)
*??????


*gen eps_within = 0
*Generate the outcome variable
gen Y2 = 3*newX + 2*T + eps_within + eps

* eps_within is a cluster-speciffic error or common shock that is i.i.d.
* eps is an idiosyncratic error that is i.i.d.

twoway (scatter Y2 newX if T==0, msize(small) mc(gray)) || ///
	(scatter Y2 newX if epsgroup==1, msize(small) mc(red)) || ///
	(lfit Y2 newX, lc(black) lw(thick)), name("`h'") ///
	title("Best Fit Regression Line with Treatment Effect") legend(order( 1 "Control" 2 "Treatment")) 
}
	
gr combine a b c d


*??????
*reg Y2 T X, cluster(epsgroup)
*??????

reg Y2 T  newX, cluster(epsgroup)
	gen BetaHat = _b[T]
	gen SEHat = _se[T]

reg Y2 T  newX

************************
**
* reg Y T `X' is a fine way to estimate Beta_hat but a wrong way to estimate SE 
* Regression analysis when observations are grouped in clusters with
* independence across clusters but correlation within clusters.
* Estimators still consistent
* Failure to control for clustering can lead to under-estimated standard errors and over-rejection using standard hypothesis tests

* treatment is not assinged at individual level
* e.g. people from same schools have many correlated charactors
* cluster at treatment level 
* for bootstrapping. bootstrap at treatment level. e.g. draw at school level instead of individual level

* https://cameron.econ.ucdavis.edu/research/Cameron_Miller_JHR_2015_February.pdf
* https://www.stata.com/meeting/mexico11/materials/cameron.pdf


***********************
* Bootstraping with clustering SE
***********************
clear
set more off
estimates clear
set seed 123
set obs 20

*Generate group-level X-variables and error terms
gen X = rnormal(0, 1)
gen eps = rnormal(0, 1)

gen epsgroup = _n
gen T = epsgroup<= (_N/2)

*More than one ob per cluster

*Make 30 observations per group
expand 30
bysort epsgroup: gen groupob = _n

*Generate deviations in X and Epsilon by period-by-observation (deviations from group levels above)
*gen x_within = 0
* https://cameron.econ.ucdavis.edu/research/Cameron_Miller_JHR_2015_February.pdf
* page 4
* https://www.stata.com/meeting/mexico11/materials/cameron.pdf
* page 7
* and https://www.itl.nist.gov/div898/handbook/ppc/section2/ppc231.htm#:~:text=A%20one%2Dway%20layout%20consists,the%20variation%20within%20each%20level
gen x_within = rnormal(0, .1)
gen newX = X + x_within


gen eps_sd= x_within
bys epsgroup: replace eps_sd=eps_sd[1]
gen eps_within = rnormal(0, abs(eps_sd)*3)

*??????
*gen eps_within = rnormal(0, abs(x_within*3))
*gen eps_within = rnormal(0, x_within*3)
*??????


*gen eps_within = 0
*Generate the outcome variable
gen Y2 = 3*newX + 2*T + eps_within + eps


reg Y2 T  newX
reg Y2 T  newX, cluster(epsgroup)

tempfile cleanfile bootfile
save `cleanfile'

*saving original BetaHat
gen Beta_real =.
gen BetaBS= .
save `bootfile'

*Run regression on bootsampled data
forvalues i = 1/10000 {
	use `cleanfile', clear
	bsample, cluster(epsgroup)
	*bsample 
	** generate a new dataset using bootstraping
   reg Y T `X'
   gen BetaBS = _b[T]
   contract BetaBS
   
   append using `bootfile'
   save `bootfile', replace

}

use `cleanfile', clear
reg Y T `X', cluster(epsgroup)
reg Y T `X'


use `bootfile', clear
kdensity BetaBS
sum BetaBS


*******************
*Another Example 
******************
* Ref: http://www.econometricsbysimulation.com/2012/10/clustering-standard-errors-state-panel.html

 *Imagine that you are trying to evaluate corporate state labor taxes as a predictor of state employment.
* First let's generate our states
clear
set seed 1033

set obs 50

gen state=_n

* Let's generate some starting values for unemployment.
gen base_employment=runiform()*.3

* Let's imagine that there is an annual trend in unemployment for each state.
gen trend=rnormal()*.025

* The policy to cut unemployment is enacted in different states around year 10.
gen policy_start = rpoisson(10)

expand 20

bysort state: gen t=_n

gen policy=(t>policy_start)

gen employment = .01*policy + base_employment + trend*t + rnormal()*.06

* The nieve regression would be to directly estimate the effect of the policy.
reg employment policy

* However, we might be concerned that the sampling is clustered.
* In order to help controlled for correlated errors by cluster we can cluster the standard errors.

* We may be interested in the interclass correlation.
***Important! 
* https://www.itl.nist.gov/div898/handbook/ppc/section2/ppc231.htm#:~:text=A%20one%2Dway%20layout%20consists,the%20variation%20within%20each%20level.
loneway employment state
* This happens to be large.

reg employment policy, cluster(state)
* This substantially increases our standard errors size and results in a failure to reject the null.
* But, in this case we know that there is an effect of the policy, should we still cluster our standard errors?

* The answer is yes, we need to cluster our standard errors.

* To show this I will simulate the data 100 times with the alternative scenario (that the null is true and there is no effect).

cap program drop cluster_test
cap program define cluster_test, rclass
clear
  set obs 50
  gen state=_n
  gen base_employment=runiform()*.3
  gen trend=rnormal()*.025
  gen policy_start = rpoisson(10)
  expand 20
  bysort state: gen t=_n
  gen policy=(t>policy_start)
  gen employment = .00*policy + base_employment + trend*t + rnormal()*.06
  * NOTE: Now the policy has no effect.
  reg employment policy
     local p1 = ttail(e(df_r), abs(_b[policy]/_se[policy]))
  return scalar sig1 = (`p1'<.05)
  reg employment policy, cluster(state)
     local p2 = ttail(e(df_r), abs(_b[policy]/_se[policy]))
  return scalar sig2 = (`p2'<.05)
end

simulate sig1=r(sig1) sig2=r(sig2), reps(100): cluster_test
sum

* sig1 is from the regression without clustered standard errors.
* sig2 is from the regression with clustered standard errors.
* We can see that both rejections too frequently reject the null (target is 5%).
* However, the difference between unclustered and clustered is the difference between falsely rejecting the null 56% of the time and 12% of the time.
* You can repeate the simulation above using 500 or 5000 states above.
* The more states you use the closer the type 1 error gets to 5%.
* However, increasing the number of years does not impove the estimates.

* There is one more thing I would like to do with this data so let's generate it once more.
cluster_test

* We may be concerned that our policy was not exogenously given to each state but rather as a product of an endogenous connection between employment and the policy.

* One method to test the exogeniety of the policy to so test if the year before the policy was enacted, if there was any predictive power on unemployment.
gen year_before=policy_start-1

gen policy_lead=(t==year_before)

reg employment policy_lead policy
* We may be tempted to not cluster the errors but clustering is just as important here as previously.

reg employment policy_lead policy, cluster(state)
* Unsurprisingly there is on evidence of endogeniety, since treatement was not endogenous by construction in this case.




