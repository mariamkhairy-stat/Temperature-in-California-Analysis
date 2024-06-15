/*
Trends in Income & Wages inequality in Egypt - ELMPS 1988-2023 
Created by: Mariam Khairy 
Creation Date: May 22 2024
Updated by:  
Updation Date: 


Version 1
*/

global  wrkdr    "C:\Users\acer\Dropbox\ELMPS 2023 Chapter\Income and Wages Paper\" // Change this into your prefered working directory
global 	data 	 "$wrkdr\Data"
global 	analysis "$wrkdr\Graphs\Wages"

/*
**# Section 3

Section 3: Wage distribution by Individual in Egypt from 1988 to 2023
●	Descriptive Section: This section explores the trend of the nominal/real  wage (hourly wages) across the timeframe starting from 1988 up till 2023.

○	Need an initial figure of real wages over time, by sex 
	Median real monthly wage growth rates (percentage per wave) by location, education, gender, sector, skill level , and formality 
○	
○	Real Wage distribution (median, box plot ) by sex, educational level, location, age group, and parent's education .
○	Real Wage distribution (median, box plot) by sector, formality, occupation, skills, economic activities, establishment  
*/


/*packages that I have used and need to be installed

*for showing the labels with values in frequesncies table 
ssc install fre
*/
use "C:\Users\acer\Dropbox\elmps 2023 rep xs 88_23 v1.0.dta", clear
**# Data Processing 

*winsorizing the wages
foreach var of varlist RhrwgCPI23 RmnthwgCPI23 RhrwgAllJobCPI23 RmnthwgAllJobCPI23 {
		sum 	`var', det 
        replace `var' = `r(p1)' if `var' < `r(p1)'
		replace `var' = `r(p99)' if `var' > `r(p99)' & `var'<.
}

*drop the armed forces from our data
ta round if usocp1d == 0
drop if usocp1d == 0
*Generation of variables that we will use in the analysis
*restriction of age between 15 & 64
gen 	age15_64 = age>=15 & age <= 64

*generate variable for sector formality & regularity for primary job
gen secformreg = .
*generate category public
replace secformreg = 1 if usinstsec == 9 | usinstsec == 10
*generate category formal private
replace secformreg = 2 if usinstsec == 8
*generate category informal private regular wage
replace secformreg = 3 if  usinstsec == 7
*generate category informal private irregular wage
replace secformreg = 4 if usinstsec == 6

la define secformreg 1 "Public" 2 "Formal Private Regular Wage" 3 "Informal Private Regular Wage" 4 "Irregular Wage"

la values secformreg secformreg
fre secformreg

*generate variable for skill level
gen skill_level =.
replace skill_level = 1 if inlist(usocp1d,1,2,3) 
replace skill_level = 2 if inlist(usocp1d,4,5,6,7) 
replace skill_level = 3 if inlist(usocp1d,8,9) 
la define skilllevel 1 "High" 2 "Medium" 3 "Low"
la values skill_level skilllevel

tab usocp1d skill_level
**# Figures
** Figure 1: Real Hourly Wages of all jobs over time, by sex

#d
gr box RhrwgAllJobCPI23 [aw = expan_indiv] if  age15_64 , 
	   nofill  over(round)  scheme(s1mono) nooutsides 
	   ytitle("Real Hourly Wage All Jobs (in 2023 L.E. using CPI)")
	   by(sex,note("Source: ELMPS 1988-2023")) note("");
#d cr	
graph 	export "$analysis\Figure1 Real Hourly Wage (All Jobs) by Round & Sex.png", replace

** Figure 2: Real Monthly Wages of (All Jobs) over time, by Sector & Formality

#d
gr box RmnthwgAllJobCPI23 [aw = expan_indiv] if  age15_64 , 
	   nofill  over(round) ylabel(#4,  labsize(small))  scheme(s1mono) nooutsides  
	   ytitle("Real Monthly Wage (in 2023 L.E. using CPI)")
	   by(secformreg,note("Source: ELMPS 1988-2023")) note("");
#d cr	

graph 	export "$analysis\Figure2 Real Monthly Wages of (All Jobs) by Round & Sector_formality.png", replace





/* 
growth rate for wage(from alljobs)
*/


**Figure 3:  Median real monthly wage growth rates (percentage per year) by location
preserve

collapse (median)  rlmnthwg_alljob = RmnthwgAllJobCPI23 ///
					if age15_64 [aw = expan_indiv],by(round urban)
reshape		wide rlmnthwg_alljob, i(urban) j(round)

foreach x in rlmnthwg_alljob {
		gen `x'1 = (((`x'1998 - `x'1988) /`x'1988) * 100) / 10
		gen `x'2 = (((`x'2006 - `x'1998) /`x'1998) * 100) / 8
		gen `x'3 = (((`x'2012 - `x'2006) /`x'2006) * 100) / 6
		gen `x'4 = (((`x'2018 - `x'2012) /`x'2012) * 100) / 6
		gen `x'5 = (((`x'2023 - `x'2018) /`x'2018) * 100) /5
}

drop 	  rl*1988 rl*1998 rl*2006 rl*2012 rl*2018 rl*2023
reshape long rlmnthwg_alljob, i(urban) j(duration 1-5)	  
label   define duration 1 "1988-1998" 2 "1998-2006" 3 "2006-2012" 4 "2012-2018" 5 "2018-2023", modify
label 	value duration duration



gr bar rlmnthwg_alljob, over(duration) over(urban, label(labs(2.5))) asyvars  bargap(0) scheme(s1mono)  bar(1, color(gs3))  bar(2, color(gs7)) bar(3, color(gs9)) bar(4, color(gs12)) bar(5, color(gs14)) intensity(inten70) blabel(bar, size(2) color(gs0) j(center) fcolor(gs16) lcolor(gs16) format("%5.1f")) ylabel( -3 "-3%" -2 "-2%" -1 "-1%"  0 "0%" 1 "1%" 2 "2%" 3 "3%" 4 "4%" 5 "5%" ,labs(small) glwidth(vthin)) ytitle("Percentage per year", size(3)) ysize(18) xsize(15)  legend( rows(2) span subtitle("") order(1 "1988-1998" 2 "1998-2006" 3 "2006-2012" 4 "2012-2018" 5 "2018-2023") textwidth(27)) title("", span  pos(11) size(3)) note("ELMPS: 1988-2023")

graph 	export "$analysis\Figure3 Real Monthly Wage (All Jobs) Growth Rate by Location.png", replace


restore




**Figure 4:  Median real monthly wage growth rates (Percentage per year) by Education
preserve

la define educlvl1 6 "University & above", modify
collapse (median)  rlmnthwg_alljob = RmnthwgAllJobCPI23 ///
					if age15_64 [aw = expan_indiv],by(round educ1)
reshape		wide rlmnthwg_alljob, i(educ1) j(round)

foreach x in rlmnthwg_alljob {
		gen `x'1 = (((`x'1998 - `x'1988) /`x'1988) * 100) / 10
		gen `x'2 = (((`x'2006 - `x'1998) /`x'1998) * 100) / 8
		gen `x'3 = (((`x'2012 - `x'2006) /`x'2006) * 100) / 6
		gen `x'4 = (((`x'2018 - `x'2012) /`x'2012) * 100) / 6
		gen `x'5 = (((`x'2023 - `x'2018) /`x'2018) * 100) /5
}
drop 	  rl*1988 rl*1998 rl*2006 rl*2012 rl*2018 rl*2023
reshape long rlmnthwg_alljob, i(educ1) j(duration 1-5)	  
label   define duration 1 "1988-1998" 2 "1998-2006" 3 "2006-2012" 4 "2012-2018" 5 "2018-2023", modify
label 	value duration duration

 

gr bar rlmnthwg_alljob, over(duration) over(educ1, label(labs(2.0)) gap(120)) asyvars bargap(0) scheme(s1mono) intensity(inten70) bar(1, color(gs3))  bar(2, color(gs7)) bar(3, color(gs9)) bar(4, color(gs12)) bar(5, color(gs13)) blabel(bar, size(1.7) color(gs0) j(center) fcolor(gs16) lcolor(gs16) format("%5.1f")) ylabel(-4 "-4%"-3 "-3%" -2 "-2%" -1 "-1%"  0 "0%" 1 "1%" 2 "2%" 3 "3%" 4 "4%" 5 "5%" 6 "6%" ,labs(vsmall) glwidth(vthin))  ytitle("Percentage per year", size(3)) ysize(25) xsize(35)  legend( rows(2) span subtitle("") order(1 "1988-1998" 2 "1998-2006" 3 "2006-2012" 4 "2012-2018" 5 "2018-2023") textwidth(27)) title("", span  pos(11) size(3)) note("ELMPS: 1988-2023")

graph 	export "$analysis\Figure4 Real Monthly Wage (All Jobs) Growth Rate by Education.png", replace


restore


**Figure 5:  Median real monthly wage growth rates (Percentage per year) by gender
preserve

collapse (median)  rlmnthwg_alljob = RmnthwgAllJobCPI23 ///
					if age15_64 [aw = expan_indiv],by(round sex)
reshape		wide rlmnthwg_alljob, i(sex) j(round)

foreach x in rlmnthwg_alljob {
		gen `x'1 = (((`x'1998 - `x'1988) /`x'1988) * 100) / 10
		gen `x'2 = (((`x'2006 - `x'1998) /`x'1998) * 100) / 8
		gen `x'3 = (((`x'2012 - `x'2006) /`x'2006) * 100) / 6
		gen `x'4 = (((`x'2018 - `x'2012) /`x'2012) * 100) / 6
		gen `x'5 = (((`x'2023 - `x'2018) /`x'2018) * 100) / 5
}
drop 	  rl*1988 rl*1998 rl*2006 rl*2012 rl*2018 rl*2023
reshape long rlmnthwg_alljob, i(sex) j(duration 1-5)	  
label   define duration 1 "1988-1998" 2 "1998-2006" 3 "2006-2012" 4 "2012-2018" 5 "2018-2023", modify
label 	value duration duration

 

gr bar rlmnthwg_alljob, over(duration) over(sex, label(labs(4.0)) gap(120)) asyvars  bargap(0) scheme(s1mono) bar(1, color(gs3))  bar(2, color(gs7)) bar(3, color(gs9)) bar(4, color(gs12)) bar(5, color(gs13)) intensity(inten70) blabel(bar, size(2.5) color(gs0) j(center) fcolor(gs16) lcolor(gs16) format("%5.1f"))  ylabel(-3 "-3%" -2 "-2%" -1 "-1%"  0 "0%" 1 "1%" 2 "2%" 3 "3%" 4 "4%"  ,labs(small) glwidth(vthin)) ytitle("Percentage per year", size(3)) ysize(25) xsize(20)  legend( rows(2) span subtitle("") order(1 "1988-1998" 2 "1998-2006" 3 "2006-2012" 4 "2012-2018" 5 "2018-2023") textwidth(27)) title("", span  pos(11) size(3)) note("ELMPS: 1988-2023")

graph 	export "$analysis\Figure5 Real Monthly Wage (All Jobs) Growth Rate by Gender.png", replace

restore


**Figure 6:  Median real monthly wage growth rates (Percentage per year) by formality & sector

preserve

collapse (median)  rlmnthwg_alljob = RmnthwgAllJobCPI23  ///
					if age15_64 [aw = expan_indiv],by(round secformreg)
reshape		wide rlmnthwg_alljob, i(secformreg) j(round)

foreach x in  rlmnthwg_alljob  {
		gen `x'1 = (((`x'1998 - `x'1988) /`x'1988) * 100) / 10
		gen `x'2 = (((`x'2006 - `x'1998) /`x'1998) * 100) / 8
		gen `x'3 = (((`x'2012 - `x'2006) /`x'2006) * 100) / 6
		gen `x'4 = (((`x'2018 - `x'2012) /`x'2012) * 100) / 6
		gen `x'5 = (((`x'2023 - `x'2018) /`x'2018) * 100) /5
}
drop 	  rl*1988 rl*1998 rl*2006 rl*2012 rl*2018 rl*2023
reshape long rlmnthwg_alljob , i(secformreg) j(duration 1-5)	  
label   define duration 1 "1988-1998" 2 "1998-2006" 3 "2006-2012" 4 "2012-2018" 5 "2018-2023", modify
label 	value duration duration

 

gr bar  rlmnthwg_alljob , over(duration) over(secformreg, label(labs(1.8)) gap(120)) asyvars bargap(0) scheme(s1mono) bar(1, color(gs3))  bar(2, color(gs7)) bar(3, color(gs9)) bar(4, color(gs12)) bar(5, color(gs13)) intensity(inten70) blabel(bar, size(1.7) color(gs0) j(center) fcolor(gs16) lcolor(gs16) format("%5.1f"))  ylabel(-3 "-3%" -2 "-2%" -1 "-1%"  0 "0%" 1 "1%" 2 "2%" 3 "3%" 4 "4%" 5 "5%" ,labs(tiny ) glwidth(vthin))  ytitle("Percentage per year", size(3)) ysize(60) xsize(75)  legend( rows(2) span subtitle("") order(1 "1988-1998" 2 "1998-2006" 3 "2006-2012" 4 "2012-2018" 5 "2018-2023") textwidth(27)) title("", span  pos(11) size(2.8)) note("ELMPS: 1988-2023")

graph 	export "$analysis\Figure6 Real Monthly Wage (All Jobs) Growth Rate by Sector & formality.png", replace


restore
**Figure 7:  Median real monthly wage growth rates (Percentage per year) by skill level

preserve

collapse (median)  rlmnthwg_alljob = RmnthwgAllJobCPI23  ///
					if age15_64 [aw = expan_indiv],by(round skill_level)
reshape		wide  rlmnthwg_alljob , i(skill_level) j(round)

foreach x in  rlmnthwg_alljob  {
		gen `x'1 = (((`x'1998 - `x'1988) /`x'1988) * 100) / 10
		gen `x'2 = (((`x'2006 - `x'1998) /`x'1998) * 100) / 8
		gen `x'3 = (((`x'2012 - `x'2006) /`x'2006) * 100) / 6
		gen `x'4 = (((`x'2018 - `x'2012) /`x'2012) * 100) / 6
		gen `x'5 = (((`x'2023 - `x'2018) /`x'2018) * 100) /5
}
drop 	  rl*1988 rl*1998 rl*2006 rl*2012 rl*2018 rl*2023
reshape long  rlmnthwg_alljob , i(skill_level) j(duration 1-5)	  
label   define duration 1 "1988-1998" 2 "1998-2006" 3 "2006-2012" 4 "2012-2018" 5 "2018-2023", modify
label 	value duration duration

 

gr bar  rlmnthwg_alljob , over(duration) over(skill_level, label(labs(2)) gap(120)) asyvars bargap(0) scheme(s1mono)  bar(1, color(gs3))  bar(2, color(gs7)) bar(3, color(gs9)) bar(4, color(gs12)) bar(5, color(gs13)) intensity(inten70) blabel(bar, size(1.7) color(gs0) j(center) fcolor(gs16) lcolor(gs16) format("%5.1f")) ylabel(-3 "-3%" -2 "-2%" -1 "-1%"  0 "0%" 1 "1%" 2 "2%" 3 "3%" 4 "4%" 5 "5%",labs(vsmall) glwidth(vthin))  ytitle("Percentage per year", size(3)) ysize(30) xsize(35)  legend( rows(2) span subtitle("") order(1 "1988-1998" 2 "1998-2006" 3 "2006-2012" 4 "2012-2018" 5 "2018-2023") textwidth(27)) title("", span  pos(11) size(2.8)) note("ELMPS: 1988-2023")

graph 	export "$analysis\Figure7 Real Monthly Wage (All Jobs) Growth Rate by Skill Level.png", replace


restore


*bar chart of the percentages of people that get less than the minimum wage
*gen minimum_wage = 