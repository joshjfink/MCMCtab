global Police_Imm "/Universe/Papers/Police_Imm/Social_Forces"

******
* Macro
	use "$Police_Imm/Data(SF)/SF_MASTER_MACRO(06_12).dta", clear
	*Drop Non-FE Countries 
	drop if cntry==208 | cntry==246 | cntry==528 | cntry==620 | cntry==152 | ///
	cntry==191 | cntry==203 | cntry==428 | cntry==705

	*Keep necessary variables 
	keep cntry year foreignpct migpct foreigndif dforeignpctnew police ///
	homicide dyn_police dyn_homicide 
	save "$Police_Imm/Data(SF)/MACROFE_SF_ISSP(03_2015).dta", replace

******
* Merge w/Micro

	***Merge Individual & Macro data 
	use "$Police_Imm/Data(SF)/MACROFE_SF_ISSP(03_2015).dta", clear
	merge 1:m cntry year using "/Universe/Papers/Police_Imm/Social_Forces/Data(SF)/SF_Extra_Countries_ISSP(06_13).dta", nogen
	drop if cntry==208 | cntry==246 | cntry==528 | cntry==620 | cntry==152 | ///
	cntry==191 | cntry==203 | cntry==410 | cntry==428 | cntry==705 | cntry== .
	save "$Police_Imm/Data(SF)/FE_Data(03_2015).dta", replace 

******
* Export CSV
	use "$Police_Imm/Data(SF)/FE_Data(03_2015).dta", clear
	keep cntry year cntryyr dspendlaw foreignpct migpct foreigndif dforeignpctnew homicide police ///
	age agesq female nevermar divorced widow kidshh rural suburb lesshs univ ///
	ptemp unemp nolabor selfemp pubemp inczscore protestant catholic otherrel ///
	foreignpct1985 egrow06 egrow96 foreignpct1996 socdem gdp issp pop liberal ///
	socdem selfemp rural dspendart dspenddef dspendedu dspendenv dspendhealth ///
	dspendret dspendunemp relreligion dyn_police dyn_homicide pfbldc gini yr2006 

	recode * (.a= .) (.b= .)

	dropmiss cntry year dspendlaw migpct foreignpct foreigndif dforeignpctnew ///
	homicide police age agesq female ptemp unemp , obs force any

	/*This is for if you want to pregroup countries
	egen cntry2= group(cntry) 
	drop cntry
	rename cntry2 cntry
	*/

	/*
	misstable summarize dspendlaw foreignpct age agesq female ///
	nevermar divorced widow kidshh rural suburb lesshs univ ///
	ptemp unemp nolabor selfemp pubemp inczscore protestant catholic otherrel relreligion
	*/
	outsheet * using "/Universe/GitHub/MCMCtab/Data/fedata.csv", comma replace
