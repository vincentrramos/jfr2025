
*****************************************************************************
/* 

Merry Now, Marry Later? Initial LM Conditions and Marital Intentions in the PHL
V. Ramos and M. Vital
Aug 2025
Replication Code - Analysis

*/


********************************************************************************
*** Preliminaries
********************************************************************************

	* Set your preferred workflow or directories after "WRITE" where to store tables and figures
	global WRITE "C:\Users\vjr1a24\OneDrive - University of Southampton\Seafile\PhD Matters\Project- GTS\_manuscripts\JFR\JFR_R1_MV\tabfig\"

	* Read the processed dataset from where it is stored 
	use "C:\Users\vjr1a24\OneDrive - University of Southampton\Seafile\PhD Matters\Project- GTS\JFR.dta", replace
	
	* For similar plot format and style, set scheme
	set scheme white_tableau			// install package first if necessary: ssc install schemepack, replace


********************************************************************************
***** Table 1
********************************************************************************

		* This chunk returns Table 1 which was not extracted into a separate file due to brevity
		table empstat [pw=weight], ///
				 statistic(count A7) ///
				 statistic(fvpercent A7)


********************************************************************************
***** Figure 1. ideal period of marriage descriptive
********************************************************************************

	*This chunk creates a boxplot if ideal period of marriage by employment status
	graph box empsta1 empsta2 empsta3 empsta4 [aw=weight]  if dist_age_mar>0 & A4==1 & A7==1 , over(gender) ///
	subtitle("Ideal period of marriage by employment status") ///
	ytitle("Years from current age") ///
	legend(label(1 "Permanent") label(2 "Fixed-term/Casual") label(3 "Unemployed") label(4 "Inactive") symxsize(6) size(3) rowgap(.5) position(6) col(4))  ///
	note("Note: Sample of never-married respondents who want to get married in the future (n=7,471), weighted")
	
		graph export "$WRITE\final_figure1_boxplotperiod.png", as(png) replace			// exports as png
		graph export "$WRITE\final_figure1_boxplotperiod.svg", as(svg) replace			// exports as svg


********************************************************************************
***** FINAL IPW MODELS (Figures 2 and 3)
********************************************************************************

*** Figure 2. Selection from original sample (Highly educated) to at-risk population (never-married)


	* Estimate the IPW model
	logit nevermarried i.empstat i.brgy i.sex ib1.birthorder i.megaregion c.A2 ib3.parent_ed i.A10 ib4.cohort [pw=weight] 	// exclude everything qual related
	predict p_nevermarried, pr
	gen ipw_m1 = 1/p_nevermarried if nevermarried == 1
	replace ipw_m1 = 10 if ipw_m1>10 & ipw_m1!=.  			// Crump et al (2009) suggest capping extreme IPWs at 10

	* Estimate the analysis model with the inverse probability weights

			logit mar_int i.empstat i.brgy i.sex ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort if A4==1 [pw=ipw_m1] // full, all
			margins, dydx(empstat) post 
			est sto log2_marint_f1_ipw
			logit mar_int i.empstat i.brgy ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort if A4==1 & 	gender=="male" [pw=ipw_m1] // full, male
			margins, dydx(empstat) post
			est sto log2_marint_f2_ipw
			logit mar_int i.empstat i.brgy ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort if A4==1 & gender=="female" [pw=ipw_m1] // full, female
			margins, dydx(empstat) post saving(marint_f3_ipw, replace)
			est sto log2_marint_f3_ipw

	* Plot the model coefficients 
	
			coefplot log2_marint_f1_ipw log2_marint_f2_ipw log2_marint_f3_ipw, drop(_cons) xline(0, lcolor(red) lwidth(medium)) ///
			xtitle("{bf: Marginal Effect on Pr(Intend to Marry)}") /// 
			graphregion(margin(medsmall)) ///
			xsize(6.5) ysize(4.5) ///
			keep(1.empstat 2.empstat 3.empstat 4.empstat) /// Keep option
			xlab(, glpattern(solid) glcolor(gs14) labsize(*0.8)) /// adds solid, light gray vertical lines at x-axis values
			grid(glpattern(solid) glcolor(gs14)) /// adds solid, light gray horizontal lines at y-axis values
			ylab(, labsize(*0.8)) /// enlarge size of y-axis labels
			baselevels  ///
			headings(1.ecactivity_und="{bf: Employment Status}", gap(0)) ///
			///msize(small)  mlcolor(navy) msymbol(square_hollow) ///
			///levels(95 90) ciopts(lcolor(navy midblue) recast(rspike rcap)) ///
			 mcolor(red blue gs8) ///
			xscale(range(-0.15 0.15)) ///
			xlabel(-0.15(0.05)0.15, angle(horizontal)) ///
			legend(label(2 "Full") label(4 "Male Sample") label(6 "Female Sample") symxsize(6) size(3) rowgap(.5) position(6) col(3))  ///
			note("Note: Models control for sex (full), age, birth order, financial satisfaction, region, urban, HEI type, coresidence, cohort, parental educ, and field of study." "Sample: Never-married respondents, excluding invalid cases (n_full=7,892; n_male=3,497; n_female=4,395), weighted using IPW", size(vsmall) span) /// add a note
			subtitle("Employment Precarity and Marital Intentions, by sex (IPW)")
			
			graph export "$WRITE\final_figure2_maritalint.png", as(png) replace 	// 	exports as png
			graph export "$WRITE\final_figure2_maritalint.svg", as(svg) replace		// 	exports as svg

*** IPW2. Selection from at-risk population (never-married) to wanting to get married


	* Estimate the IPW model
	logit mar_int i.empstat i.brgy i.sex ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort if A4==1 [pw=weight]
	predict p_marint, pr
	gen ipw_m2 = 1/p_marint if mar_int == 1
	replace ipw_m2 = 10 if ipw_m2>10 & ipw_m2!=.  			// crump et al (2009): replace ipw_m1>10 to 10

	* Estimate the analysis model with the inverse probability weights

			reg dist_age_mar i.empstat i.brgy i.sex ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort [pw=ipw_m2] if dist_age_mar>0 & A4==1 & A7==1
			margins, dydx(empstat) post 
			est sto reg2_period_f1_ipw
			reg dist_age_mar i.empstat i.brgy ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort [pw=ipw_m2] if dist_age_mar>0 & A4==1 & A7==1 & gender=="male"
			margins, dydx(empstat) post 
			est sto reg2_period_f2_ipw
			reg dist_age_mar i.empstat i.brgy ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort [pw=ipw_m2] if dist_age_mar>0 & A4==1 & A7==1 & gender=="female"
			margins, dydx(empstat) post 
			est sto reg2_period_f3_ipw

	* Plot the model coefficients 

			coefplot reg2_period_f1_ipw reg2_period_f2_ipw reg2_period_f3_ipw, drop(_cons) xline(0, lcolor(red) lwidth(medium)) ///
			xtitle("{bf: Marginal Effect on Ideal Period of Marriage (in years)}") /// 
			graphregion(margin(medsmall)) ///
			xsize(6.5) ysize(4.5) ///
			keep(1.empstat 2.empstat 3.empstat 4.empstat) /// Keep option
			xlab(, glpattern(solid) glcolor(gs14) labsize(*0.8)) /// adds solid, light gray vertical lines at x-axis values
			grid(glpattern(solid) glcolor(gs14)) /// adds solid, light gray horizontal lines at y-axis values
			ylab(, labsize(*0.8)) /// enlarge size of y-axis labels
			baselevels  ///
			headings(1.ecactivity_und="{bf: Employment Status}", gap(0)) ///
			subtitle(, color(black) fcolor(gs15) lcolor(gs12)) ///
			xscale(range(-0.5 1.5)) ///
			xlabel(-0.5(0.5)1.5, angle(horizontal)) ///
			legend(label(2 "Full") label(4 "Male Sample") label(6 "Female Sample") symxsize(6) size(3) rowgap(.5) position(6) col(3))  ///
			note("Note: Models control for sex (full), age, birth order, financial satisfaction, region, urban, HEI type, coresidence, cohort, parental educ, and field of study." "Sample: Never-married who want to get married in the future, excluding invalid cases (n_full=7,155; n_male=3,178; n_female=3,977), weighted using IPW", size(vsmall) span) /// add a note
			subtitle("Employment Precarity and Ideal Period of Marriage, by sex (IPW)")

			graph export "$WRITE\final_figure3_idealperiod.png", as(png) replace	// 	exports as png
			graph export "$WRITE\final_figure3_idealperiod.svg", as(svg) replace	// 	exports as svg

		
********************************************************************************
***** FINAL SUPPLEMENETARY MATERIAL
* These codes replicate the supplementary material
********************************************************************************


*** Appendix Figure 1. ideal age at marriage

	*bysort empstat gender: sum A8 [aw=weight] if A4==1 & A7==1 & dist_age_mar >0
	*separate A8, by(empstat) gen(empst)
	
	* Create a boxplot for ideal age at marriage
	graph box empst1 empst2 empst3 empst4 [aw=weight]  if dist_age_mar >0 & A4==1 & A7==1, over(gender)  ///
	subtitle("Ideal age of marriage by employment status") ///
	legend(label(1 "Permanent") label(2 "Fixed-term/Casual") label(3 "Unemployed") label(4 "Inactive") symxsize(6) size(3) rowgap(.5)) ///
	subtitle("Ideal age at marriage by employment status") ///
	ytitle("Expressed ideal age") ///
	legend(label(1 "Permanent") label(2 "Fixed-term/Casual") label(3 "Unemployed") label(4 "Inactive") symxsize(6) size(3) rowgap(.5) position(6) col(4))  ///
	note("Note: Sample of never-married respondents who want to get married in the future (n=7,471), weighted")
	
		graph export "$WRITE\final_appendixfigure1_boxplotperiod.png", as(png) replace	// 	exports as png
		graph export "$WRITE\final_appendixfigure1_boxplotperiod.svg", as(svg) replace	// 	exports as svg

*** Appendix Table 1. Descriptives for analytical sample

	* Run first the complete case model to generate the sample indicator
	logit mar_int i.empstat i.brgy i.sex ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort if A4==1 // full, all
	gen _sample=1 if e(sample)

	* Obtain descriptive statistics then restore to full dataset
	preserve
	keep if _sample==1

	table (var) [pw=weight], ///
			 statistic(fvrawfrequency mar_int empstat brgy sex birthorder finsat megaregion discipline_cat HEI_type_rev parent_ed A10 cohort) ///
			 statistic(fvpercent mar_int empstat brgy sex birthorder finsat megaregion discipline_cat HEI_type_rev parent_ed A10 cohort)
			 
	collect recode result fvrawfrequency = column1 fvpercent = column2
	collect layout (var) (result[column1 column2])
	collect style header result, level(hide)
	collect style row stack, nobinder spacer
	collect preview
	collect export "WRITE\desc.xlsx", replace

	restore 


*** Appendix Table 2. full regression tables

	*** Marital Intentions
	* Run first the IPW model
	logit nevermarried i.empstat i.brgy i.sex ib1.birthorder i.megaregion c.A2 ib3.parent_ed i.A10 ib4.cohort [pw=weight] 	// exclude everything qual related
	predict p_nevermarried, pr
	gen ipw_m1 = 1/p_nevermarried if nevermarried == 1
	replace ipw_m1 = 10 if ipw_m1>10 & ipw_m1!=.  			// crump et al (2009): cap IPW at 10 to limit extreme weights

		* Run analysis models and export using outreg2. If outreg2 is not yet installed, install first using ssc install outreg2		
		logit mar_int i.empstat if A4==1 [pw=ipw_m1] // full, all
		margins, dydx(*) post 
		outreg2 using "$WRITE\tables_reg.xls", replace bracket ctitle(Intentions, Full) label bdec(3) sdec(3) alpha(0.01, 0.05, 0.1) symbol(***, **, *)
		
		logit mar_int i.empstat i.brgy i.sex ib1.birthorder i.finsat i.megaregion c.A2 ib3.parent_ed i.A10 ib4.cohort if A4==1 [pw=ipw_m1] // full, all
		margins, dydx(*) post 
		outreg2 using "$WRITE\tables_reg.xls", append bracket ctitle(Intentions, Full) label bdec(3) sdec(3) alpha(0.01, 0.05, 0.1) symbol(***, **, *)
		
		logit mar_int i.empstat i.brgy i.sex ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort if A4==1 [pw=ipw_m1] // full, all
		margins, dydx(*) post 
		outreg2 using "$WRITE\tables_reg.xls", append bracket ctitle(Intentions, Full) label bdec(3) sdec(3) alpha(0.01, 0.05, 0.1) symbol(***, **, *)
		
		logit mar_int i.empstat i.brgy ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort if A4==1 & 	gender=="male" [pw=ipw_m1] // full, male
		margins, dydx(*) post
		outreg2 using "$WRITE\tables_reg.xls", append bracket ctitle(Intentions, Males) label bdec(3) sdec(3) alpha(0.01, 0.05, 0.1) symbol(***, **, *)

		logit mar_int i.empstat i.brgy ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort if A4==1 & gender=="female" [pw=ipw_m1] // full, female
		margins, dydx(*) post saving(marint_f3_ipw, replace)
		outreg2 using "$WRITE\tables_reg.xls", append bracket ctitle(Intentions, Females) label bdec(3) sdec(3) alpha(0.01, 0.05, 0.1) symbol(***, **, *)

	*** Ideal Period at Marriage
	* Run first the IPW model
	logit mar_int i.empstat i.brgy i.sex ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort if A4==1 [pw=weight]
	predict p_marint, pr
	gen ipw_m2 = 1/p_marint if mar_int == 1
	replace ipw_m2 = 10 if ipw_m2>10 & ipw_m2!=.  			// crump et al (2009): replace ipw_m1>10 to 10

		* Run analysis models and export using outreg2. If outreg2 is not yet installed, install first using ssc install outreg2		
		reg dist_age_mar i.empstat [pw=ipw_m2] if dist_age_mar>0 & A4==1 & A7==1
		margins, dydx(*) post 
		outreg2 using "$WRITE\tables_reg.xls", append bracket ctitle(Period, Full) label bdec(3) sdec(3) alpha(0.01, 0.05, 0.1) symbol(***, **, *)
		
		reg dist_age_mar i.empstat i.brgy i.sex ib1.birthorder i.finsat i.megaregion c.A2 ib3.parent_ed i.A10 ib4.cohort [pw=ipw_m2] if dist_age_mar>0 & A4==1 & A7==1
		margins, dydx(*) post 
		outreg2 using "$WRITE\tables_reg.xls", append bracket ctitle(Period, Full) label bdec(3) sdec(3) alpha(0.01, 0.05, 0.1) symbol(***, **, *)

		reg dist_age_mar i.empstat i.brgy i.sex ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort [pw=ipw_m2] if dist_age_mar>0 & A4==1 & A7==1
		margins, dydx(*) post 
		outreg2 using "$WRITE\tables_reg.xls", append bracket ctitle(Period, Full) label bdec(3) sdec(3) alpha(0.01, 0.05, 0.1) symbol(***, **, *)
		
		reg dist_age_mar i.empstat i.brgy ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort [pw=ipw_m2] if dist_age_mar>0 & A4==1 & A7==1 & gender=="male"
		margins, dydx(*) post 
		outreg2 using "$WRITE\tables_reg.xls", append bracket ctitle(Period, Males) label bdec(3) sdec(3) alpha(0.01, 0.05, 0.1) symbol(***, **, *)
		
		reg dist_age_mar i.empstat i.brgy ib1.birthorder i.finsat i.megaregion i.discipline_cat i.HEI_type_rev c.A2 ib3.parent_ed i.A10 ib4.cohort [pw=ipw_m2] if dist_age_mar>0 & A4==1 & A7==1 & gender=="female"
		margins, dydx(*) post 
		outreg2 using "$WRITE\tables_reg.xls", append bracket ctitle(Period, Females) label bdec(3) sdec(3) alpha(0.01, 0.05, 0.1) symbol(***, **, *)