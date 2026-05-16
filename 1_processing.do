

*****************************************************************************
/* 

Merry Now, Marry Later? Initial LM Conditions and Marital Intentions in the PHL
V. Ramos and M. Vital
Aug 2025
Replication Code - Data Processing

*/


********************************************************************************
*** Preliminaries
********************************************************************************

	clear all
	set more off

	* Set your preferred workflow or directories after "WRITE" where to store tables and figures
	global WRITE "C:\Users\vjr1a24\OneDrive - University of Southampton\Seafile\PhD Matters\Project- GTS\_manuscripts\JFR\JFR_R1_MV\tabfig\"

	* Read the ORIGINAL dataset from the Commission on Higher Education as named and stored
	use "C:\Users\vjr1a24\OneDrive - University of Southampton\Seafile\PhD Matters\Project- GTS\gts_weights.dta", replace
	


* Marital Intentions Variable
	recode A7 (0 26 = .) (7 8  = .)
	gen mar_int = A7
	recode mar_int (2=0)

* Current Marital Status A4
	recode A4 (0=.)
	tab A4 [aw=weight]
	
		
* Process age at marriage
	sum A8, det
	replace A8 = . if A8 < 20 | A8> 48 // OUTLIERS and nonsensical values
	replace A8 = . if A8 < 25  // OUTLIERS and nonsensical values

* Ideal age at marriage
	fre A2
	gen dist_age_mar = A8 - A2
	sum dist_age_mar
		
*Employment status
	gen empstat = .
		replace empstat = 1 if D1==1 | D2==1
		replace empstat = 2 if empstat==1 & (D13==2 | D13==3)
		replace empstat = 3 if (D1!=1 & D2!=1) & D3==1 & (D6==1 & D7==1) // not working + looking + available and willing
		replace empstat = 4 if empstat==. & (D1!=1 & D2!=1) & D3==2 & (D4>=2 & D4<=5) & (D6==1 & D7==1) // not working + not looking because of weather, temporary illness, awaiting results, waiting for rehire +  available and willing
		replace empstat = 4 if empstat==.
	lab def empstat 1 "Permanent Employed" 2 "Casual/Fixed-Term" 3 "Unemployed" 4 "Inactive", replace
	lab val empstat empstat


* Observation IDs and other identifiers
	gen OBS_id = _n
	order OBS_id
		sum OBS_id // ordering based on raw data
		label var OBS_id "Observation ID, based on raw data"

	// using IDENTIFIERS
	tostring GTS_IDNUM1, gen(id_1) format(%02.0f)  
	tostring GTS_IDNUM2, gen(id_2) format(%04.0f)  

	gen str6 ID = id_1 + id_2
	label var ID "Unique individual ID"
	order ID, after(OBS_id)
	duplicates report ID // check if there are any duplicates
	drop id_1 id_2
			

* label megaregion variable
	lab def megaregion 1 "Northern Luzon" 2 "Central and Southern Luzon" 3 "Visayas Island" 4 "Northern Mindanao" 5 "Southern Mindanao"
	lab val megaregion megaregion
	lab var megaregion "Regional group"
	

* Create birth cohort variable
gen cohort = .
	replace cohort = 1 if A1_YY < 1980 // 1979 and earlier
	replace cohort = 2 if A1_YY > 1979 & A1_YY <= 1985 // 1980 - 1984
	replace cohort = 3 if A1_YY > 1985 & A1_YY <= 1990 // 1985 - 1990
	replace cohort = 4 if A1_YY > 1989 & A1_YY < 1994 // 1991 to 1993
	
	fre cohort
	label define cohort 1 "1979 and earlier" 2 "1980 - 1984" 3 "1985 - 1990" 4 "1991 to 1993", replace
	lab val cohort cohort
	
	
	
* Calculate age of marriage (year of marriage MINUS year of birth)
	fre A5_YY	
	recode A5_YY (9998 = .) // missing
		gen agemar = A5_YY - A1_YY 
		sum agemar
		label var agemar "Age of Marriage"
		fre agemar
		
* Marital status: dummy var = 1, if never married
	fre A4
		gen nevermarried = .
			replace nevermarried = 1 if A4 ==1
			replace nevermarried = 0 if A4 > 1 & A4 ~=.
			tab A4 nevermarried, m
		lab var nevermarried "Dummy = 1 if never been married"
	
		
* Parental education
	lab define edustat 1 "Primary" 2 "Secondary" 3 "Tertiary", replace
	
	// Father's Education (highest attainment)
	gen fath_ed = .
		replace fath_ed = 1 if A12A == 0 | A12A == 1 
		replace fath_ed = 1 if A12A >= 11 & A12A <= 18 
		replace fath_ed = 2 if A12A >= 21 & A12A <= 25 
		replace fath_ed = 3 if A12A >= 31 & A12A <= 32
		replace fath_ed = 3 if A12A >= 41 & A12A <= 47
		replace fath_ed = 3 if A12A == 51
		lab val fath_ed  edustat		
		
	// Mother's Education
	gen moth_ed = .
		replace moth_ed = 1 if A12B == 0 | A12B == 1 
		replace moth_ed = 1 if A12B >= 11 & A12B <= 18 
		replace moth_ed = 2 if A12B >= 21 & A12B <= 25 
		replace moth_ed = 3 if A12B >= 31 & A12B <= 32
		replace moth_ed = 3 if A12B >= 41 & A12B <= 47
		replace moth_ed = 3 if A12B == 51
		lab val moth_ed  edustat		
		
	// COMBINED: highest level of parental education
	egen parent_ed = rowmax(moth_ed fath_ed)
	lab val parent_ed edustat
	fre parent_ed
	
	label var fath_ed "Father's level of educ attainment"
	label var moth_ed "Mother's level of educ attainment"
	label var parent_ed "Highest level of educ attainment of either parents"
		

* gender
	encode gender, gen(sex)
	lab var sex "Sex at birth"


* Birth order
	gen siborder = .
		replace siborder=1 if A14F==1 & A14A_02==.
		replace siborder=2 if A14F==1 & A14A_02==2
		replace siborder=3 if A14F==2
		replace siborder=4 if A14F==3
		replace siborder=5 if A14F>=4
	lab def siborder 1 "Only Child" 2 "Eldest" 3 "2nd" 4 "3rd" 5 "4th or higher", replace
	lab val siborder siborder
	lab var siborder "Sibling Order"

* HEI Type
	encode hei_type, gen(hei_typ) 
	lab var hei_typ "Type of higher education institution"
	
** Discipline
	encode discipline_grp, gen(discipline_cat)
	lab var discipline_cat "Degree of study"

	
* Save dataset as JFR.dta either in $WRITE\ or in your preferred folder like below:

	save "C:\Users\vjr1a24\OneDrive - University of Southampton\Seafile\PhD Matters\Project- GTS\JFR.dta", replace
