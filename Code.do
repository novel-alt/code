*************************************************************************************
clear
use data
*************************************************************************************
#delimit ;
global controls "age age_sq edu farmtime health party labor plant_demons farmland_area consolidation certification income agtraing emtraing insurance loan water coll_economic distance capabilities drought frost credit policy technology_support" ;
#delimit cr
*************************************************************************************
//4.1 Benchmark regression results
*(1)~(2) OLS
reg relilience digital
reg relilience digital $controls
*(3) DDML,rf
global Y relilience
global D digital
global X $controls
set seed 42
ddml init partial, kfolds(5)
ddml E[Y|X]: pystacked $Y $X, type(reg) method(rf)
ddml E[D|X]: pystacked $D $X, type(reg) method(rf)
ddml crossfit
ddml estimate, robust
*************************************************************************************
//4.2 Robustness test
//4.2.1 Substituting key variables
*Degree of digital empowerment
global Y relilience
global C digital
global X $controls
set seed 42
ddml init partial, kfolds(5)
ddml E[Y|X]: reg $Y $X
ddml E[D|X]: reg $C $X
ddml crossfit
ddml estimate, robust
*************************************************************************************
//4.2.2 addressing endogenous issue
*partial linear IV
gen byte touse = !missing($D)
bysort villege: egen totMPf1 = total($D) if touse==1
sum totMPf1
by villege: egen cMPf1 = count($D) if touse==1
generate avgMPf1 = (totMPf1 - $D) / (cMPf1 - 1)
global Z avgMPf1
ddml init iv, kfolds(5)
ddml E[Y|X]: pystacked $Y $X, type(reg) method(rf)
ddml E[Z|X]: pystacked $Z $X, type(reg) method(rf)
ddml E[D|X]: pystacked $D $X, type(reg) method(rf)
ddml crossfit
ddml estimate, robust
*sensitivity approach
sensemakr relilience digital $controls , ///
 treat(digital) gbenchmark(labor farmland_area income insurance credit policy) gname(all) contourplot
 treat(digital) gbenchmark(labor farmland_area income insurance credit policy) gname(all) tcontourplot
//4.2.3 Sample re-processing
*(1)Trimmed treatment-1%/5%
winsor2 relilience digital $controls, replace cuts(1 99) trim
winsor2 relilience digital $controls, replace cuts(5 95) trim
*(2)Sample reselection
*drop if county==2,4,5,6,7,8,9,13,16,18,19
global Y relilience
global D digital
global X $controls
set seed 42
ddml init partial, kfolds(5)
ddml E[Y|X]: pystacked $Y $X, type(reg) method(rf)
ddml E[D|X]: pystacked $D $X, type(reg) method(rf)
ddml crossfit
ddml estimate, robust
*************************************************************************************
//4.2.4 Redesigning the DDML model
*Sample splitting ratio (1:2)
ddml init partial, kfolds(3)
ddml E[Y|X]: pystacked $Y $X, type(reg) method(rf)
ddml E[D|X]: pystacked $D $X, type(reg) method(rf)
ddml crossfit
ddml estimate, robust
*Sample splitting ratio (1:7)
ddml init partial, kfolds(8)
ddml E[Y|X]: pystacked $Y $X, type(reg) method(rf)
ddml E[D|X]: pystacked $D $X, type(reg) method(rf)
ddml crossfit
ddml estimate, robust
*interactive model
ddml init interactive, kfolds(5)
ddml E[Y|X,D]: pystacked $Y $X, type(reg) method(rf)
ddml E[D|X]: pystacked $D $X, type(reg) method(rf)
ddml crossfit
ddml estimate, trim(0.35) robust
*different algorithm
*method(gradboost) 
ddml init partial, kfolds(5)
ddml E[Y|X]: pystacked $Y $X, type(reg) method(gradboost)
ddml E[D|X]: pystacked $D $X, type(reg) method(gradboost)
ddml crossfit
ddml estimate, robust
*method(nnet) 
ddml init partial, kfolds(5)
ddml E[Y|X]: pystacked $Y $X, type(reg) method(nnet)
ddml E[D|X]: pystacked $D $X, type(reg) method(nnet)
ddml crossfit
ddml estimate, robust
*************************************************************************************
//5.1 Mechanism analysis
bootstrap r(ind_eff) r(dir_eff), reps(5000) : sgmediation relilience , mv( social_captical ) iv( digital ) 
bootstrap r(ind_eff) r(dir_eff), reps(5000) : sgmediation relilience , mv( information_channel ) iv( digital ) 
bootstrap r(ind_eff) r(dir_eff), reps(5000) : sgmediation relilience , mv( public_service_equal ) iv( digital ) 
*************************************************************************************
//5.2 Heterogeneity analysis
keep if inlist(income_class,1) 
keep if inlist(income_class,0)
keep if inlist(education_class,1)  
keep if inlist(education_class,2) 
keep if inlist(education_class,3)  
keep if inlist(publicservice_class,1) 
keep if inlist(publicservice_class,0) 
*ddml
global Y relilience
global D digital
global X $controls
set seed 42
ddml init partial, kfolds(5) 
ddml E[Y|X]: pystacked $Y $X, type(reg) method(rf)
ddml E[D|X]: pystacked $D $X, type(reg) method(rf)
ddml crossfit
ddml estimate, robust
*************************************************************************************
*5.3 Mechanism analysis under Heterogeneity analysis
keep if inlist(income_class,1) 
keep if inlist(income_class,0) 
keep if inlist(education_class,1) 
keep if inlist(education_class,2) 
keep if inlist(education_class,3)  
keep if inlist(publicservice_class,1) 
keep if inlist(publicservice_class,0) 
*mechanism
bootstrap r(ind_eff) r(dir_eff), reps(5000) : sgmediation relilience , mv( social_captical ) iv( digital ) 
bootstrap r(ind_eff) r(dir_eff), reps(5000) : sgmediation relilience , mv( information_channel ) iv( digital ) 
bootstrap r(ind_eff) r(dir_eff), reps(5000) : sgmediation relilience , mv( public_service_equal ) iv( digital ) 
