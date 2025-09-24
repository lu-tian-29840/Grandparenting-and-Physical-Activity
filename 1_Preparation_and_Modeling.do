capture log close
log using lutian-randhrs03-v3, replace text

//   Program: grandparenting and physcial activity
//   Task: Vairable creation and modeling analysis

*** Establish models for weighted physical activity and 
*** grandparenting, age, sex, income #6 (08/16)
*** Create tables for physical activity by grandparenting,  
**** mean age of 5 grandparenting groups, create physical activity index variable(08/03) 
//   Project: Grandparenting and phydical activity
//   Author:lutian 08/24/2020

version 17
clear all
set linesize 80
matrix drop _all
set scheme s1color
//   #1
//   Load data
use rand_hrs_03, clear

***Choose variables
keep HHID HHIDPN OSUBHH_H                             	 		 /// household id
     H12ITOT H12AHOUS  H12HHRES RAEDYRS          		          /// ses
     RADYEAR   R12AGEY_E                                 		 /// death and age
     RAGENDER female                                    		 ///sex
     RARACEM RAHISPAN black white hispanic other        		 ///race
     RABPLACE                                            		 ///born
     R12MSTAT married_t12 sep_div_t12 widowed_t12 never_mar_t12  ///marr
     R12CESD R12SHLT srh_t12                           			 ///self-rated health
     R12HIBPE R12DIABE R12CANCRE R12LUNGE R12HEARTE R12STROKE    ///disease 
     R12SMOKEN R12DRINK                                			 ///smoke drink
     OE046 OE060 OE063 OE120 OA099 OA100 OX061_MC OX063_MC        ///
     hh_grandchild non_hh_grandchild hundred reschildren         ///
     nongrand grandparent lcnonres icnonres multicores skipgen   ///GP
     R12VGACTX R12MDACTX R12LTACTX                   			 ///physical activity
     adl_imp_t12 mob_lim_t12                 		             ///impairments
	 R12WTHH R12WTRESP RAESTRAT RAEHSAMP						 //Survey weights (Household Weight, Respondent Weight, Std Error Stratum, and Stratum Half-Sample Code)
	 
	 
tab  grandparent if OE046 == 0 
* 141
count if grandparent == 0 & OE046 == 0
summarize R12AGEY_E if grandparent == 0 & OE046 == 0 
* 58
summarize R12AGEY_E if grandparent == 0 & OE046 != 0 
* 63
	
	 bysort HHID: gen hh_size = _N 
     tab hh_size
tab OSUBHH_H  

tab hh_size OA100
*Set svy variables：
svyset RAEHSAMP [pweight=R12WTHH], strata(RAESTRAT)  vce(linearized)
	 
*Check the missing
mdesc

*Descriptives
summarize 

ttest  R12AGEY_E, by(nongrand)

summarize OE063

//  #2
//  Two-way tables of phydical activity across grandparenting types

* table of each kind of phydical activity
tab1 R12VGACTX R12MDACTX R12LTACTX, m

* table of each kind of grandparenting
tab1 grandparent nongrand lcnonres icnonres multicores skipgen, m

pwcorr black white, sig

/*
***Tables of vigrous physical activity across grandparenting
tab R12VGACTX nongrand, m
tab R12VGACTX grandparent, m
tab R12VGACTX lcnonres, m
tab R12VGACTX icnonres, m
tab R12VGACTX multicores, m
tab R12VGACTX skipgen, m

***Tables of moderate physical activity across grandparenting
tab R12MDACTX nongrand, m
tab R12MDACTX grandparent, m
tab R12MDACTX lcnonres, m
tab R12MDACTX icnonres, m
tab R12MDACTX multicores, m
tab R12MDACTX skipgen, m

***Tables of lite physical activity across grandparenting
tab R12LTACTX nongrand, m
tab R12LTACTX grandparent, m
tab R12LTACTX lcnonres, m
tab R12LTACTX icnonres, m
tab R12LTACTX multicores, m
tab R12LTACTX skipgen, m         */

/*/   #3
//   t test to see the age difference for each grandparenting type
tab R12AGEY_E, m

*t test for nongrandparet
ttest R12AGEY_E, by(nongrand)

*t test for grandparent
ttest R12AGEY_E, by(grandparent)
anova R12AGEY_E grandparent

*t test for light care non-coresidential grandparenting care
ttest R12AGEY_E, by(lcnonres)

*t test for intensive care non-coresidential grandparenting care
ttest R12AGEY_E, by(icnonres)

*t test for multigenerational coresidential grandparenting care
ttest R12AGEY_E, by(multicores)

*t test for skip generation household grandparenting care
ttest R12AGEY_E, by(skipgen)

//   #4 
//   Relationship between age and phydical activity

*Relationship between age and vigrous phydical activity
anova R12AGEY_E R12VGACTX

*Relationship between age and moderate phydical activity
anova R12AGEY_E R12MDACTX

*Relationship between age and light phydical activity
anova R12AGEY_E R12LTACTX

*/

//   #5
//   create physical activity index variable

*Recode vigrous physical activity
clonevar r12vgactx_v = R12VGACTX
recode   r12vgactx_v 5=0 4=1 3=2 2=3 1=4 
tab1     R12VGACTX r12vgactx_v, m

*Recode moderate physical activity
clonevar r12mdactx_m = R12MDACTX
recode   r12mdactx_m 5=0 4=1 3=2 2=3 1=4 
tab1     R12MDACTX r12mdactx_m, m

*Recode light physical activity
clonevar r12ltactx_l = R12LTACTX
recode   r12ltactx_l 5=0 4=1 3=2 2=3 1=4 
tab1     R12LTACTX r12ltactx_l, m

*weighting each kind of physical activity
gen r12vgactx_w = r12vgactx_v * 1.8 
gen r12mdactx_w = r12mdactx_m * 1.4
gen r12ltactx_w = r12ltactx_l * 1.2
gen r12acts_w = r12vgactx_w + r12mdactx_w + r12ltactx_w



*** Y: r12acts_w
*Check the distribution of weighted physical activity (the dependent var)
tab r12acts_w, m
hist r12acts_w


tab nongrand, m
tab grandparent, m

gen     gp=.
replace gp=0 if grandparent==0
replace gp=1 if lcnonres==1
replace gp=2 if icnonres==1
replace gp=3 if multicores==1
replace gp=4 if skipgen==1

label define gplab2 0"nongrandparent" 1"non-coresidential light care" 2"non-resident intensive" 3"multigen hhld" 4"skipped gen intensive"            
label values gp gplab2
tab gp

* histogram r12acts_w, by (gp)              

graph bar (mean) r12acts_w, over(gp, lab(angle(45)))   ///
      bargap(-30)                                      ///
      ytitle("Mean of Physical Activityty")   ///
      title("Mean of Physical Activity")      ///
	  subtitle("by Grandparenting Types")              ///
	  note("Source: 2014 wave of the Health and Retirement Study (N = 17,851)") ///
	  bar(1, color(eltblue))


* se for variables in graph

bysort gp: egen N = count(r12acts_w)
bysort gp: egen SD = sd(r12acts_w)
generate SE = SD / sqrt(N)	
                    

ttest r12acts_w, by(lcnonres)
ttest r12acts_w, by(icnonres)
ttest r12acts_w, by(multicores)
ttest r12acts_w, by(skipgen)  

*Recode H12ITOT
gen h12itot2 = log(H12ITOT)
tab h12itot2, m
sum h12itot2

*Recode H12AHOUS
*Careful, log of zero is undefined. Use inverse hyperbolic sine as an alternative. 
gen h12ahous2 = asinh(H12AHOUS)
tab h12ahous2, m
sum h12ahous2

* Create the centered age			  
summarize R12AGEY_E
gen centered_r12agey_e = R12AGEY_E- r(mean)	
tab centered_r12agey_e, m

mdesc		r12acts_w                                                       ///
                    nongrand lcnonres icnonres multicores skipgen         ///
                    female centered_r12agey_e white black hispanic other  ///
			        sep_div_t12 widowed_t12 never_mar_t12  married_t12    ///
				    RAEDYRS h12itot2 h12ahous2                            ///
				    adl_imp_t12 mob_lim_t12 srh_t12  
local control1 ///
                    r12acts_w                                             ///
                    nongrand lcnonres icnonres multicores skipgen         ///
                    female centered_r12agey_e white black hispanic other  ///
			        sep_div_t12 widowed_t12 never_mar_t12  married_t12    ///
				    h12itot2 h12ahous2  srh_t12                           ///

foreach v in `control1' {
	
drop if       `v'==.
}					


local control2 ///
                  RAEDYRS adl_imp_t12 mob_lim_t12
				  
				  
foreach v in `control2' {
	
drop if       `v'==.m
}					  
				  
				  
svy: mean r12acts_w                                                       ///
                    nongrand lcnonres icnonres multicores skipgen         ///
                    female centered_r12agey_e white black hispanic other  ///
			        sep_div_t12 widowed_t12 never_mar_t12  married_t12    ///
				    RAEDYRS h12itot2 h12ahous2                            ///
				    adl_imp_t12 mob_lim_t12 srh_t12          
	

anova r12acts_w nongrand lcnonres icnonres multicores skipgen 
ttest r12acts_w, by(nongrand) /// 8.07
ttest r12acts_w, by(lcnonres) /// 6.81
ttest r12acts_w, by(icnonres) ///  7.05
ttest r12acts_w, by(multicores) ///  6.57
ttest r12acts_w, by(skipgen)  /// 6.33

 black white hispanic other
tabstat  R12AGEY_E, by(gp) stat(mean)
tabstat  srh_t12 , by(gp) stat(mean)

***********       Part 1

//  #6
//  Model 0: Intercept only, empty model, to check the unconditional means

reg c.r12acts_w


//   #7
//   Model 1: Regression on weighted physical activity by grandparenting categotires

* Y: r12acts_w
* x: lcnonres icnonres multicores skipgen 

*** Non-grandparent is the reference group. 
** Reason why remove grandparent: collinearity

reg r12acts_w  i.lcnonres i.icnonres i.multicores i.skipgen 

    
//  #8
//  Model 2: Regression on weighted physical activity by demographics

* Y: r12acts_w
* x: Sex (Female)
*    Age (in years), R12AGEY_E
*    Race/Ethnicity (Hispanic, Black, Other). White is reference. 
*    Marital Status (Sep_div_12, widowed _12 , never_mar _12 ).Married is reference. 


tab1 RAGENDER female, m
tab1 RARACEM RAHISPAN  


reg r12acts_w i.female R12AGEY_E i.black i.hispanic i.other ///
              i.sep_div_t12 i.widowed_t12 i.never_mar_t12      
             
	
//   #9
//   Model 3: M1 + M2, Grandparenting + Demographics 

reg r12acts_w i.lcnonres i.icnonres i.multicores i.skipgen /// m1
              i.female R12AGEY_E i.black i.hispanic i.other ///m2
              i.sep_div_t12 i.widowed_t12 i.never_mar_t12 //m2



* Regression using centered age	plus others 
reg r12acts_w i.lcnonres i.icnonres i.multicores i.skipgen /// m1
              i.female centered_r12agey_e i.black i.hispanic i.other ///m2
              i.sep_div_t12 i.widowed_t12 i.never_mar_t12 //m2


//   #10
//   Model 4: M3 + SES	

* Y: r12acts_w	  
* x: Grandparenting + Demographics 
*	 education, in years (RAEDYRS)             | 
*    Household Income (H12ITOT)                |  SES
*    Value of primary residence (H12AHOUS)     |
	  
tab RAEDYRS, m
sum H12ITOT, detail


//  #11
//  Model 4 
reg r12acts_w i.lcnonres i.icnonres i.multicores i.skipgen /// gp
              i.female R12AGEY_E i.black i.hispanic i.other /// demo
              i.sep_div_t12 i.widowed_t12 i.never_mar_t12   /// demo 
			  RAEDYRS h12itot2 h12ahous2                   // SES

* Regression using centered age	plus others 
reg r12acts_w i.lcnonres i.icnonres i.multicores i.skipgen /// gp
              i.female centered_r12agey_e i.black i.hispanic i.other /// demo
              i.sep_div_t12 i.widowed_t12 i.never_mar_t12   /// demo 
			  RAEDYRS h12itot2 h12ahous2                   // SES
			  
			  
*** Check VIF

vif
			  
//   #12
//   Model 5 = Model 4 + Health and Mobility Impairments

*  Y: r12acts_w
*  x: Grandparenting + Demographics + SES            | Health
*     Count of ADL Impairments (adl_imp_t12)         | and
*     Count of Mobility Limitations (mob_lim_t12)    | Mobility
*     Self-rated Health (srh_t12=6)                  | Impairments


tab1 R12CESD R12SHLT srh_t12, m

reg r12acts_w i.lcnonres i.icnonres i.multicores i.skipgen  /// gp
              i.female R12AGEY_E i.black i.hispanic i.other /// demo
              i.sep_div_t12 i.widowed_t12 i.never_mar_t12   /// demo 
			  RAEDYRS h12itot2 h12ahous2                   /// SES
			  adl_imp_t12 mob_lim_t12 srh_t12           // impairments

* Regression using centered age	plus others 			  
reg r12acts_w i.lcnonres i.icnonres i.multicores i.skipgen  /// gp
              i.female centered_r12agey_e i.black i.hispanic i.other /// demo
              i.sep_div_t12 i.widowed_t12 i.never_mar_t12   /// demo 
			  RAEDYRS h12itot2 h12ahous2                   /// SES
			  adl_imp_t12 mob_lim_t12 srh_t12           // impairments	
			  


*** Run a regression where I test whether the effect of grandparenting on PA varies 
*** by race, controlling for other variables.


gen     race=. 
replace race=1 if white==1    & black==0 & hispanic==0 & other==0
replace race=2 if black==1    & white==0 & hispanic==0 & other==0
replace race=3 if hispanic==1 & white==0 & black==0    & other==0
replace race=4 if other==1    & white==0 & black==0    & hispanic==0



label define racelab 1"White" 2"Black" 3"Hispanic" 4"Others"             
label values race racelab
tab race

** generate a new categorical variable about grandparenting, wherein non-grandparent
** is the reference group.

gen     gpc=.
replace gpc=0 if grandparent==0
replace gpc=1 if lcnonres==1
replace gpc=2 if icnonres==1
replace gpc=3 if multicores==1
replace gpc=4 if skipgen==1

label define gpclab 0"nongrandparent" 1"light care non-coresidential" 2"intensive care non-coresidential" 3"multigenerational" 4"skipgenerational"            
label values gpc gpclab
tab gpc

tabstat R12AGEY_E, by(gpc) stat(mean)
			  
//   #13 			  
***  Nested OLS regression of weighted physical activities on focal and controls		  
			  
nestreg: reg r12acts_w (i.lcnonres i.icnonres i.multicores i.skipgen)      ///
                       (i.female centered_r12agey_e i.black i.hispanic i.other) ///
					   (i.sep_div_t12 i.widowed_t12 i.never_mar_t12)   ///
					   (RAEDYRS h12itot2 h12ahous2)                    ///
                       (adl_imp_t12 mob_lim_t12 srh_t12)


					
**  Obtain the nested regression model table
asdoc reg r12acts_w	i.lcnonres i.icnonres i.multicores i.skipgen, nest ///
                    dec(2) replace

asdoc reg r12acts_w	i.lcnonres i.icnonres i.multicores i.skipgen           ///
                    i.female centered_r12agey_e i.black i.hispanic i.other ///
					, dec(2) nest 
					
asdoc reg r12acts_w i.lcnonres i.icnonres i.multicores i.skipgen           ///
                    i.female centered_r12agey_e i.black i.hispanic i.other ///
				    i.sep_div_t12 i.widowed_t12 i.never_mar_t12, dec(2) nest 
					
	
asdoc reg r12acts_w i.lcnonres i.icnonres i.multicores i.skipgen           ///
                    i.female centered_r12agey_e i.black i.hispanic i.other ///
 				    i.sep_div_t12 i.widowed_t12 i.never_mar_t12            ///
					RAEDYRS h12itot2 h12ahous2, dec(2) nest 
					
asdoc reg r12acts_w i.lcnonres i.icnonres i.multicores i.skipgen           ///
                    i.female centered_r12agey_e i.black i.hispanic i.other ///
 				    i.sep_div_t12 i.widowed_t12 i.never_mar_t12            ///
					RAEDYRS h12itot2 h12ahous2                             ///		
	                adl_imp_t12 mob_lim_t12 srh_t12, dec(2) nest          
	                

					
					

//   #14
//   "Survey weighted"* Nested OLS regression and create tables
					
***  "Survey weighted"* Nested OLS regression of weighted physical activities 
***  on focal and controls		  
			  
summarize centered_r12agey_e


*Recode H12ITOT
gen h12itot2 = log(H12ITOT)
tab h12itot2, m
sum h12itot2 

*Recode H12AHOUS
*Careful, log of zero is undefined. Use inverse hyperbolic sine as an alternative. 
gen h12ahous2 = asinh(H12AHOUS)
tab h12ahous2, m
sum h12ahous2


					

	

**  14 a) Weighted Descriptives for manuscript

svy: mean r12acts_w                                             ///
                    grandparent lcnonres icnonres multicores skipgen     ///
                    female centered_r12agey_e white black hispanic other   ///
			        sep_div_t12 widowed_t12 never_mar_t12  married_t12            ///
				    RAEDYRS h12itot2 h12ahous2                               ///
				    adl_imp_t12 mob_lim_t12 srh_t12                       

		  
** 14 b) Weighted OLS for manuscript	  

nestreg: svy: reg   r12acts_w                                                  ///
                    (i.lcnonres i.icnonres i.multicores i.skipgen)           ///
                    (i.female centered_r12agey_e i.black i.hispanic i.other) ///
			        (i.sep_div_t12 i.widowed_t12 i.never_mar_t12)            ///
				    (RAEDYRS h12itot2 h12ahous2)                             ///
				    (adl_imp_t12 mob_lim_t12 srh_t12)
        
					
**14 c) histogram of each grandparenting and physical activities for manuscript

local newgp ///
      grandparent lcnonres icnonres multicores skipgen

gen newgrand = .
local i = 1
foreach v in `newgp' {
replace newgrand = `i' if `v' == 1
label define newgrand `i' "`v'", add
local ++i
}
label values newgrand newgrand
list, noobs clean

tab newgrand

graph bar r12acts_w, over (newgrand)


***********       Part 2
//   Create two sedentary  categories
* # 1. Create sed1. complete sedentariness, dependent variable for BRM1
tab r12acts_w, m

gen     sed1 = 0
replace sed1 = 1 if r12acts_w == 0
replace sed1 = . if r12acts_w == .

tab1 r12acts_w sed1, m

* Create sed2, dependent variable variable for BRM2
recode r12acts_w (0/2.4=1) (2.6/17.6=0), gen(sed2)
tab1   r12acts_w sed2, m

 
***   Create tables for supplementary table 1 (=Table 3)
***   SBRM M0: Create binary regression model of intercept-only 

svy: logit sed1
estimates store sbrm1m0



***   SBRM1 M1: Binary regression model by grandparenting categories

svy: logit sed1 i.lcnonres i.icnonres i.multicores i.skipgen, or nolog
estimates store sbrm1m1



***   SBRM1 M2: Create binary regression model by demographics

svy: logit sed1 i.lcnonres i.icnonres i.multicores i.skipgen  ///
                i.female centered_r12agey_e i.black i.hispanic i.other, or nolog			  
estimates store sbrm1m2



***  SBRM1 M3: Binary regression model by demographics+ grandparenting categories

svy: logit  sed1 i.lcnonres i.icnonres i.multicores i.skipgen            ///
                 i.female centered_r12agey_e i.black i.hispanic i.other ///
				 i.sep_div_t12 i.widowed_t12 i.never_mar_t12, or nolog			  
estimates store sbrm1m3
			  	  



***  SBRM1 M4: M3 + SES 

svy: logit sed1 i.lcnonres i.icnonres i.multicores i.skipgen            /// gp
                i.female centered_r12agey_e i.black i.hispanic i.other /// demo
                i.sep_div_t12 i.widowed_t12 i.never_mar_t12            /// demo 
		        RAEDYRS h12itot2 h12ahous2, or nolog                   // SES

estimates store sbrm1m4		  



*** SBRM1 M5: M4 + Health and Mobility Impairments

svy:logit sed1 i.lcnonres i.icnonres i.multicores i.skipgen            /// gp
               i.female centered_r12agey_e i.black i.hispanic i.other /// demo
               i.sep_div_t12 i.widowed_t12 i.never_mar_t12            /// demo 
		       RAEDYRS h12itot2 h12ahous2                             /// SES
	           adl_imp_t12 mob_lim_t12 srh_t12, or nolog             //impairments
			  

estimates store sbrm1m5  				
					
					
esttab sbrm1m1 sbrm1m2 sbrm1m3 sbrm1m4 sbrm1m5, star ///
       eform ci(2) obslast  ///
       mtitles wide nogap  b(2) compress 
	   
	   
***   Create tables for supplementary table 1 (=Table 4)
***   SBRM2 M0: Create binary regression model of intercept-only 

svy: logit sed2
estimates store sbrm2m0



***   SBRM2 M1: Binary regression model by grandparenting categories

svy: logit sed2 i.lcnonres i.icnonres i.multicores i.skipgen, or nolog
estimates store sbrm2m1



***   SBRM2 M2: Create binary regression model by demographics

svy: logit sed2 i.lcnonres i.icnonres i.multicores i.skipgen  ///
                i.female centered_r12agey_e i.black i.hispanic i.other, or nolog			  
estimates store sbrm2m2



***  SBRM2 M3: Binary regression model by demographics+ grandparenting categories

svy: logit  sed2 i.lcnonres i.icnonres i.multicores i.skipgen  /         //
                 i.female centered_r12agey_e i.black i.hispanic i.other ///
				 i.sep_div_t12 i.widowed_t12 i.never_mar_t12, or nolog			  
estimates store sbrm2m3
			  	  



***  SBRM2 M4: M3 + SES 

svy: logit sed2 i.lcnonres i.icnonres i.multicores i.skipgen            /// gp
                i.female centered_r12agey_e i.black i.hispanic i.other /// demo
                i.sep_div_t12 i.widowed_t12 i.never_mar_t12            /// demo 
		        RAEDYRS h12itot2 h12ahous2, or nolog                   // SES

estimates store sbrm2m4		  



*** SBRM2 M5: M4 + Health and Mobility Impairments

svy:logit sed2 i.lcnonres i.icnonres i.multicores i.skipgen  /// gp
               i.female centered_r12agey_e i.black i.hispanic i.other /// demo
               i.sep_div_t12 i.widowed_t12 i.never_mar_t12   /// demo 
		       RAEDYRS h12itot2 h12ahous2                   /// SES
	           adl_imp_t12 mob_lim_t12 srh_t12, or nolog   //impairments
			  

estimates store sbrm2m5  				
					
	
					
esttab sbrm2m1 sbrm2m2 sbrm2m3 sbrm2m4 sbrm2m5, star ///
       eform ci(2) obslast  ///
       mtitles wide nogap  b(2) compress 
	   
