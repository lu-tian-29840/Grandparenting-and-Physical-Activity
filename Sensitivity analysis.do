* Contains three major sensitivity analyses
* 1) Consider mobile limiation (line 109)
* 2) interaction test by race or gender 
* 3) Turkey test

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
     R13VGACTX R13MDACTX R13LTACTX                   			 ///physical activity
	 R12VGACTX R12MDACTX R12LTACTX     ///
     adl_imp_t12 mob_lim_t12                 		             ///impairments
	 R12WTHH R12WTRESP RAESTRAT RAEHSAMP	
	 
	 
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



clonevar r13vgactx_v = R13VGACTX
recode   r13vgactx_v 5=0 4=1 3=2 2=3 1=4 
tab1     R13VGACTX r13vgactx_v, m

*Recode moderate physical activity
clonevar r13mdactx_m = R13MDACTX
recode   r13mdactx_m 5=0 4=1 3=2 2=3 1=4 
tab1     R13MDACTX r13mdactx_m, m

*Recode light physical activity
clonevar r13ltactx_l = R13LTACTX
recode   r13ltactx_l 5=0 4=1 3=2 2=3 1=4 
tab1     R13LTACTX r13ltactx_l, m

*weighting each kind of physical activity
gen r13vgactx_w = r13vgactx_v * 1.8 
gen r13mdactx_w = r13mdactx_m * 1.4
gen r13ltactx_w = r13ltactx_l * 1.2
gen r13acts_w = r13vgactx_w + r13mdactx_w + r13ltactx_w



*** Table 2
*** Models for reviwers' opinion
*** Model 1 the equation with caregiving plus what you are calling controls: 
* gender and marital status. Then add blocks of variables that allow you to test hypotheses. 
*** Model 2 + race/ethncity, 
*** Model 3 + SES,
*** Model 4 + health.

svy:reg r12acts_w i.lcnonres i.icnonres i.multicores i.skipgen ///
	i.female i.sep_div_t12 i.widowed_t12 i.never_mar_t12  centered_r12agey_e /// demo
	
	estimates store Model1
	
svy:reg r12acts_w i.lcnonres i.icnonres i.multicores i.skipgen ///
	i.female i.sep_div_t12 i.widowed_t12 i.never_mar_t12  centered_r12agey_e ///  demo
	i.black i.hispanic i.other ///  demo
	
	estimates store Model2
	
svy:reg r12acts_w i.lcnonres i.icnonres i.multicores i.skipgen ///
	i.female i.sep_div_t12 i.widowed_t12 i.never_mar_t12  centered_r12agey_e ///  demo
	i.black i.hispanic i.other ///  dem 	
	RAEDYRS h12itot2 h12ahous2 ///ses
	
	estimates store Model3


svy:reg r12acts_w i.lcnonres i.icnonres i.multicores i.skipgen ///
	i.female i.sep_div_t12 i.widowed_t12 i.never_mar_t12  centered_r12agey_e ///  demo
	i.black i.hispanic i.other ///  dem 	
	RAEDYRS h12itot2 h12ahous2 ///ses
	mob_lim_t12 srh_t12  
	
	estimates store Model4

esttab Model1 Model2 Model3 Model4 using my_table.doc, replace se




*** interaction
svy:reg r13acts_w gpc##race                                  /// gp##race
              i.female centered_r12agey_e                   /// demo
              i.sep_div_t12 i.widowed_t12 i.never_mar_t12   /// demo 
			  RAEDYRS h12itot2 h12ahous2                   /// SES
			  mob_lim_t12 srh_t12               // impairments	
			  
			  
svy: reg r13acts_w i.gpc##i.sex  /// gp
              centered_r12agey_e i.black i.hispanic i.other /// demo
              i.sep_div_t12 i.widowed_t12 i.never_mar_t12   /// demo 
			  RAEDYRS h12itot2 h12ahous2                   /// SES
			   mob_lim_t12 srh_t12           // impairments	

*** Tukey test
	  anova r12acts_w gp
	  pwcompare gp, mcompare(tukey)
/*                                                        |                                   Tukey
                                                        |   Contrast   Std. err.     [95% conf. interval]
--------------------------------------------------------+------------------------------------------------
                                                     gp |
        non-coresidential light care vs nongrandparent  |  -1.388126   .0873135     -1.626326   -1.149927
              non-resident intensive vs nongrandparent  |  -.1693583   .1088457     -.4662998    .1275832
                       multigen hhld vs nongrandparent  |  -1.516291   .1577212      -1.94657   -1.086013
               skipped gen intensive vs nongrandparent  |  -1.751162   .2179755      -2.34582   -1.156504
non-resident intensive vs non-coresidential light care  |   1.218768   .0947998      .9601451    1.477391
         multigen hhld vs non-coresidential light care  |  -.1281652   .1483765     -.5329506    .2766202
 skipped gen intensive vs non-coresidential light care  |  -.3630354   .2113124     -.9395159     .213445
               multigen hhld vs non-resident intensive  |  -1.346933   .1619855     -1.788845    -.905021
       skipped gen intensive vs non-resident intensive  |  -1.581803   .2210806     -2.184933   -.9786742
                skipped gen intensive vs multigen hhld  |  -.2348702   .2488076     -.9136414    .4439009
--------------------------------------------------------------------------------------------------------- 
*/



