

*===================================================================================================================================================
* Project : 	USPCALING GROUNDNUT PRODUCTIVITY
* Program:		LEARNING (TRAINING) AND GROUNDNUT PRODUCTIVITY IN GHANA, MALI AND NIGERIA
* ==================================================================================================================================================
 
 *This was perfored in STATA 17
 
clear all
set maxvar 30000
set more off


****************************************************************************************************************************************************
* SET FILE PATHS
**************************************************

local path "../"


*gl data "`path'data"

*gl Results "`path'tables"


** Loading the data**
use "data_for_analysis", clear

****************************************************************************************************************************************************
*DATA CLEANING AND PREPARATION
****************************************************************************************************************************************************
foreach var of varlist typsoil1 typsoil2 typsoil3 nbrhoejour {
replace `var'=0 if `var' ==.
}




 foreach v of var country region district villag {
        	encode `v', gen(`v'_1)
        }
		
	
		
***Generating variables of interest***		
gen association = (cooperative==2|cooperative==3) //Cooperative membership

gen extension_priv = (extension > 0) //Private extension

gen extension_pub = (visitpublic > 0) //Public extension

gen extension_access = (extension_priv==1| extension_pub==1) //joint extension

gen extension_both = (extension_priv==1 & extension_pub==1)


gen training = formagri //agaricultural training

gen training_g = (formarrach==1| formarrach==2|formarrach==3)  //groundnut training (production, aflatoxin mgt)

***Generating outcome variables***
gen objective_sale = (typeagri == 1)  //objective of groudnut productioon is for sales

gen objective_consumption = (typeagri ==2) //objective of groudnut production is for consumption

gen credit_cash = credite //access to cash credit for production

gen credit_kind = creditn //access to credit in kind - seeds, fertilizers, pesticides, equipments

gen intercropping = cmixt //intercropping

gen crop_rotation = crotation //crop rotation

gen organic_fert = (qteorga > 0) //Organic fertilizers

gen inorganic_fert = (qtechimi > 0) //Inorganic fertilizers

gen fertilisers = (organic_fert==1| inorganic_fert==1) // fertilisers

gen pesticide = (literpesti > 0) //Pesticide

gen herbicide = (litherbi > 0) //Herbicide

gen fungicide = (literfongi > 0) //Fungicide



gen ihs_pest = asinh(literpesti)

gen ihs_org = asinh(organic_fert)

gen ihs_chem = asinh(inorganic_fert)

****************************************************************************************************************************************************
****GENERATING TIME AVERAGES FOR CRE MODEL*****
egen agebar = mean(age), by (id)
egen nbschoolbar = mean( nbschool), by (id)
egen hhsizebar  = mean(hhsize), by (id)
egen cooperativebar = mean(cooperative), by (id)
egen formagribar = mean(formagri), by (id)
egen formarrachbar = mean(formarrach), by (id)
egen visitpublicbar = mean(visitpublic), by (id)
egen extensionbar = mean(extension), by (id)
egen creditebar = mean(credite), by (id)
egen creditnbar = mean( creditn), by (id)
egen crotationbar = mean(crotation), by (id)
egen cmixtbar = mean(cmixt), by (id)
egen nbrhoejourbar = mean(nbrhoejour), by (id)
egen upricebar = mean(uprice), by (id)
egen cseed_habar = mean(cseed_ha), by (id)
egen cfert_habar = mean(cfert_ha), by (id)
egen cpest_habar = mean(cpest_ha), by (id)
egen clabor_habar = mean(clabor_ha), by (id)
egen gsizebar = mean(gsize), by (id)
egen off_farmbar = mean(off_farm), by (id)
egen dratiobar = mean(dratio), by (id)
egen adoptbar = mean(adopt), by (id)


**************************************************************************************************************************************************
*****GLOBAL VARIABLES*******

global inputs adopt organic_fert inorganic_fert fertilisers pesticide credit_cash credit_kind

global practices intercropping crop_rotation adopt organic_fert

global interest extension_access

global xlist age sexe nbschool hhsize dmurbain dmvillage association clabor_ha gsize off_farm dratio typsoil1 typsoil2 typsoil3

global tlist agebar nbschoolbar hhsizebar  gsizebar off_farmbar dratiobar

/*VAriable Labels for SM design*/
*************************************************************************************************************************************
label var adopt "Adoption dummy"
label var improvsup "Area under adoption (ha)"
label var age "Age of household head (years)"
label var sexe "Sex of household head (dummy, male=1)"
label var nbschool "Education level (Number of years)"
label var hhsize "Household size (number of persons)"
label var cooperative "Farmers group membership (dummy) "
label var formagri "Training on agriculture (dummy)"
label var formarrach "Training on groundnut farming (dummy)"
label var visitpublic "Public agricultural extension service (number of visits)"
label var extension "Private agricultural extension service (number of visits)"
label var credite "Cash credit for groundnut farming (dummy)"
label var creditn "Credit in kind for groundnut farming (dummy)"
label var dmurbain "Distance to the nearest urban market (km)"
label var dmvillage "Distance the nearest village market (km)"
label var crotation "Crop rotation (dummy)"
label var cmixt "Mixed Crops (dummy)"
label var nbrhoejour "Labor force (man.day)"
label var uprice "Unit selling price (USD/kg)"
label var cseed_ha "Seed cost (USD/ha)"
label var cfert_ha "Fertilizer cost (USD/ha)"
label var cpest_ha "Pesticide cost (USD/ha)"
label var clabor_ha "Labor cost (USD/ha)"
label var gsize "Groundnut area (ha)"
label var off_farm "Off-farm income (dummy)"
label var dratio "Dependency ratio"
label var typsoil1 "Clay soil (dummy)"
label var typsoil2 "Sandy-clay soil (dummy)"
label var typsoil3 "Silty soil (dummy)"
label var assoc " Cooperative membership (dummy)"
label var extension_access "Extension access (dummy, Yes=1)"
label var association "Cooperative membership (dummy)"
label var extension_priv "Private extension (dummy)"
label var extension_pub "Public extension (dummy)"
label var training "Training (dummy)"
label var extension_both "Public-private extension (dummy)"

***************************************************************************************************************************************************
**Table 1: Summary Statistics**


graph hbar (mean) adopt adopt fertilisers pesticide credit_cash intercropping crop_rotation, showyvars legend(off) scheme(economist) xsize(20) ysize(15) graphregion(color(white)) bgcolor(white)


graph bar (mean) adopt adopt fertilisers pesticide credit_cash intercropping crop_rotation, over(country) showyvars legend(off) scheme(economist) xsize(20) ysize(15) graphregion(color(white)) bgcolor(white)

/*=========Kdensity of gprod gyield*/ 
		
****COOPERATIVE MEMBERSHIP****
tabstat gyield gprod sellers qsale sales_value prod_value, by(adopt)

twoway 	(kdensity gyield if association==1, lwidth(medium) lcolor(black) lpattern(solid)) /// 
		(kdensity gyield if association==0, lwidth(medium) lcolor(red) lpattern(dash)), ///
		ytitle("Density") xtitle("Groundnut yield (kg/ha)") ///
		xline(587.74, lpattern(solid) lwidth(thin) lcolor(black)) ///
		text(0 `=587.74' "Non-cooperative members", color(black) j(left) size(vsmall) place(nw) orient(vertical)) /// 
		xline(918.86, lpattern(solid) lwidth(thin) lcolor(red)) ///
		text(0 `=918.86' "Cooperative members", color(red) j(left) size(vsmall) place(nw) orient(vertical)) ///
		legend (label(1 "Non-cooperative members") label(2 "Cooperative members")) saving(Yield, replace)
		
		graph export "Yield.png", as(png) replace

twoway 	(kdensity gprod if association==1 & gprod<5000, lwidth(medium) lcolor(black) lpattern(solid)) /// 
		(kdensity gprod if association==0 & gprod<5000, lwidth(medium) lcolor(red) lpattern(dash)), ///
		ytitle("Density") xtitle("Groundnut production (kg)") ///
		xline(880.43, lpattern(solid) lwidth(thin) lcolor(black)) ///
		text(0 `=880.43' "Non-cooperative members", color(black) j(left) size(vsmall) place(nw) orient(vertical)) /// 
		xline(1608.06, lpattern(solid) lwidth(thin) lcolor(red)) ///
		text(0 `=1608.06' "Cooperative members", color(red) j(left) size(vsmall) place(nw) orient(vertical)) ///
		legend (label(1 "Non-cooperative members") label(2 "Cooperative members")) saving(Prod, replace)
		
		graph export "Prod.png", as(png) replace

	gr combine Yield.gph Prod.gph , col(2) iscale(.5) commonscheme
 
graph export "outcomes.png", as(png) replace	
		
		
****EXTENSION ACCESS****
twoway 	(kdensity gyield if extension==1, lwidth(medium) lcolor(black) lpattern(solid)) /// 
		(kdensity gyield if extension==0, lwidth(medium) lcolor(red) lpattern(dash)), ///
		ytitle("Density") xtitle("Groundnut yield (kg/ha)") ///
		xline(587.74, lpattern(solid) lwidth(thin) lcolor(black)) ///
		text(0 `=587.74' "Non-cooperative members", color(black) j(left) size(vsmall) place(nw) orient(vertical)) /// 
		xline(918.86, lpattern(solid) lwidth(thin) lcolor(red)) ///
		text(0 `=918.86' "Cooperative members", color(red) j(left) size(vsmall) place(nw) orient(vertical)) ///
		legend (label(1 "Non-cooperative members") label(2 "Cooperative members")) saving(Yield2, replace)
		
		graph export "Yield.png", as(png) replace

twoway 	(kdensity gprod if association==1 & gprod<5000, lwidth(medium) lcolor(black) lpattern(solid)) /// 
		(kdensity gprod if association==0 & gprod<5000, lwidth(medium) lcolor(red) lpattern(dash)), ///
		ytitle("Density") xtitle("Groundnut production (kg)") ///
		xline(880.43, lpattern(solid) lwidth(thin) lcolor(black)) ///
		text(0 `=880.43' "Non-cooperative members", color(black) j(left) size(vsmall) place(nw) orient(vertical)) /// 
		xline(1608.06, lpattern(solid) lwidth(thin) lcolor(red)) ///
		text(0 `=1608.06' "Cooperative members", color(red) j(left) size(vsmall) place(nw) orient(vertical)) ///
		legend (label(1 "Non-cooperative members") label(2 "Cooperative members")) saving(Prod2, replace)
		
		graph export "Prod2.png", as(png) replace
	

gr combine Yield2.gph Prod.gph , col(2) iscale(.5) commonscheme
 
graph export "outcomes2.png", as(png) replace
		

/************Descriptive stats */

eststo clear
by year association, sort: eststo: quietly estpost sum $xlist assoc
esttab using sum_stat.csv, replace cell(mean(fmt(2)) sd(par fmt(2))) p(2) label nodepvar

eststo clear
by association, sort: eststo: quietly estpost sum $xlist assoc
esttab using sum_stat2.csv, replace cell(mean(fmt(2)) sd(par fmt(2))) p(2) label nodepvar





*****PANEL REGRESSIONS******
xtset id year
	
***Result2: Extension impacts on practices***	

xtreg intercropping $interest $xlist $tlist i.year i. district_1, re r
	outreg2 using Result2.tex, replace ctitle (Intercropping) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_access) ctitle (Intercropping (DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab replace
	
	
xtreg intercropping $interest $xlist $tlist i.year, re r
	outreg2 using Result2.tex, append ctitle (Intercropping) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_access) ctitle (Intercropping)  long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg crop_rotation $interest $xlist $tlist i.year i. district_1, re r
	outreg2 using Result2.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_access) ctitle (Crop Rotation (DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg crop_rotation $interest $xlist $tlist i.year, re r
	outreg2 using Result2.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_access) ctitle (Crop Rotation) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	

xtreg adopt $interest $xlist $tlist i.year i. district_1, re r
	outreg2 using Result2.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) 
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_access) ctitle (Improved Seeds (DFE))  long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg adopt $interest $xlist $tlist i.year, re r
	outreg2 using Result2.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_access) ctitle (Improved Seeds) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
xtreg organic_fert $interest $xlist $tlist i.year i. district_1, re r
	outreg2 using Result2.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_access) ctitle (Organic Fertilisers (DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg organic_fert $interest $xlist $tlist i.year, re r
	outreg2 using Result2.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_access) ctitle (Organic Fertilisers) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	

***Result3: Public and private extension on inputs***

xtreg intercropping extension_priv extension_pub $xlist $tlist i.year i. district_1, re r
	outreg2 using Result3.tex, replace ctitle (Intercropping) dec(3) lab tex(frag pr land) keep(extension_priv extension_pub $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_priv extension_pub) ctitle (Intercropping(DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg intercropping extension_priv extension_pub $xlist $tlist i.year, re r
	outreg2 using Result3.tex, append ctitle (Intercropping) dec(3) lab tex(frag pr land) keep(extension_priv extension_pub $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_priv extension_pub) ctitle (Intercropping)  long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg crop_rotation extension_priv extension_pub $xlist $tlist i.year i. district_1, re r
	outreg2 using Result3.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep(extension_priv extension_pub $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_priv extension_pub) ctitle (Crop Rotation(DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg crop_rotation extension_priv extension_pub $xlist $tlist i.year, re r
	outreg2 using Result3.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep(extension_priv extension_pub $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_priv extension_pub) ctitle (Crop Rotation) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	

xtreg adopt extension_priv extension_pub $xlist $tlist i.year i. district_1, re r
	outreg2 using Result3.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep(extension_priv extension_pub $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) 
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_priv extension_pub) ctitle (Improved Seeds(DFE))  long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg adopt extension_priv extension_pub $xlist $tlist i.year, re r
	outreg2 using Result3.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep(extension_priv extension_pub $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_priv extension_pub) ctitle (Improved Seeds) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
xtreg organic_fert extension_priv extension_pub $xlist $tlist i.year i. district_1, re r
	outreg2 using Result3.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep(extension_priv extension_pub $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_priv extension_pub) ctitle (Organic Fertilisers(DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg organic_fert extension_priv extension_pub $xlist $tlist i.year, re r
	outreg2 using Result3.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep(extension_priv extension_pub $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_priv extension_pub) ctitle (Organic Fertilisers) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	

***Result6: Public-private extension on inputs***



xtreg intercropping extension_both $xlist $tlist i.year i. district_1, re r
	outreg2 using Result4.tex, replace ctitle (Intercropping) dec(3) lab tex(frag pr land) keep(extension_both $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_both) ctitle (Intercropping(DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg intercropping extension_both $xlist $tlist i.year, re r
	outreg2 using Result4.tex, append ctitle (Intercropping) dec(3) lab tex(frag pr land) keep(extension_both $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_both) ctitle (Intercropping)  long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg crop_rotation extension_both $xlist $tlist i.year i. district_1, re r
	outreg2 using Result4.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep(extension_both $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_both) ctitle (Crop Rotation(DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg crop_rotation extension_both $xlist $tlist i.year, re r
	outreg2 using Result4.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep(extension_both $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_both) ctitle (Crop Rotation) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	

xtreg adopt extension_both $xlist $tlist i.year i. district_1, re r
	outreg2 using Result4.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep(extension_both $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) 
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_both) ctitle (Improved Seeds(DFE))  long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg adopt extension_both $xlist $tlist i.year, re r
	outreg2 using Result4.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep(extension_both $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_both) ctitle (Improved Seeds) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
xtreg organic_fert extension_both $xlist $tlist i.year i. district_1, re r
	outreg2 using Result4.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep(extension_both $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_both) ctitle (Organic Fertilisers(DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg organic_fert extension_both $xlist $tlist i.year, re r
	outreg2 using Result4.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep(extension_both $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (extension_both) ctitle (Organic Fertilisers) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
*****Result7: Training and inputs***

xtreg intercropping training $xlist $tlist i.year i. district_1, re r
	outreg2 using Result5.tex, replace ctitle (Intercropping) dec(3) lab tex(frag pr land) keep(training $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (training) ctitle (Intercropping(DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg intercropping training $xlist $tlist i.year, re r
	outreg2 using Result5.tex, append ctitle (Intercropping) dec(3) lab tex(frag pr land) keep(training $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (training) ctitle (Intercropping)  long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg crop_rotation training $xlist $tlist i.year i. district_1, re r
	outreg2 using Result5.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep(training $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (training) ctitle (Crop Rotation(DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg crop_rotation training $xlist $tlist i.year, re r
	outreg2 using Result5.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep(training $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (training) ctitle (Crop Rotation(DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	

xtreg adopt training $xlist $tlist i.year i. district_1, re r
	outreg2 using Result5.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep(training $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) 
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (training) ctitle (Improved Seeds(DFE))  long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg adopt training $xlist $tlist i.year, re r
	outreg2 using Result5.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep(training $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (training) ctitle (Improved Seeds) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
xtreg organic_fert training $xlist $tlist i.year i. district_1, re r
	outreg2 using Result5.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep(training $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (training) ctitle (Organic Fertilisers(DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg organic_fert training $xlist $tlist i.year, re r
	outreg2 using Result5.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep(training $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (training) ctitle (Organic Fertilisers) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
***Results9: Interaction between training and farm practices

xtreg intercropping extension_access#training $xlist $tlist i.year i. district_1, re r
	outreg2 using Result6.tex, replace ctitle (Intercropping) dec(3) lab tex(frag pr land) keep(1.extension_access#1.training $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (1.extension_access#1.training) ctitle (Intercropping(DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg intercropping extension_access#training $xlist $tlist i.year, re r
	outreg2 using Result6.tex, append ctitle (Intercropping) dec(3) lab tex(frag pr land) keep(1.extension_access#1.training $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (1.extension_access#1.training) ctitle (Intercropping)  long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg crop_rotation extension_access#training $xlist $tlist i.year i. district_1, re r
	outreg2 using Result6.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep(1.extension_access#1.training $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (1.extension_access#1.training) ctitle (Crop Rotation(DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg crop_rotation extension_access#training $xlist $tlist i.year, re r
	outreg2 using Result6.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep(1.extension_access#1.training $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (1.extension_access#1.training) ctitle (Crop Rotation) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	

xtreg adopt extension_access#training $xlist $tlist i.year i. district_1, re r
	outreg2 using Result6.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep(1.extension_access#1.training $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) 
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (1.extension_access#1.training) ctitle (Improved Seeds(DFE))  long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg adopt extension_access#training $xlist $tlist i.year, re r
	outreg2 using Result6.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep(1.extension_access#1.training $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (1.extension_access#1.training) ctitle (Improved Seeds) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
xtreg organic_fert extension_access#training $xlist $tlist i.year i. district_1, re r
	outreg2 using Result6.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep(1.extension_access#1.training $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (1.extension_access#1.training) ctitle (Organic Fertilisers(DFE)) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append
	
	
xtreg organic_fert extension_access#training $xlist $tlist i.year, re r
	outreg2 using Result6.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep(1.extension_access#1.training $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	outreg2 using Result1_2, sideway stats(coef se aster  ci_low ci_high ) keep (1.extension_access#1.training) ctitle (Organic Fertilisers) long  nocons nor2 noobs noni  nonotes noparen dec(3) quote lab append

	

***ROBUSTNESS CHECKS





***Result10: Extension impacts on practices (Household FE Estimator)***	

xtreg intercropping $interest $xlist $tlist i.year i. district_1, fe r
	outreg2 using Result_rc_1.tex, replace ctitle (Intercropping) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	
xtreg intercropping $interest $xlist $tlist i.year, fe r
	outreg2 using Result_rc_1.tex, append ctitle (Intercropping) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	
	
	
xtreg crop_rotation $interest $xlist $tlist i.year i. district_1, fe r
	outreg2 using Result_rc_1.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	
	
	
xtreg crop_rotation $interest $xlist $tlist i.year, fe r
	outreg2 using Result_rc_1.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	
	

xtreg adopt $interest $xlist $tlist i.year i. district_1, fe r
	outreg2 using Result_rc_1.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) 
	
	
	
	
xtreg adopt $interest $xlist $tlist i.year, fe r
	outreg2 using Result_rc_1.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	
	
xtreg organic_fert $interest $xlist $tlist i.year i. district_1, fe r
	outreg2 using Result_rc_1.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	
	
	
xtreg organic_fert $interest $xlist $tlist i.year, fe r
	outreg2 using Result_rc_1.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
	




**********************************


***Result12: Hausman Taylor estimation

xthtaylor intercropping $interest $xlist , endog($interest)
	outreg2 using Result_rc_2.tex, replace ctitle (Intercropping) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	

	

	
	
	
xthtaylor crop_rotation $interest $xlist , endog($interest)
	outreg2 using Result_rc_2.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	
	
	

	

xthtaylor adopt $interest $xlist , endog($interest)
	outreg2 using Result_rc_2.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) 
	
	
	
	

	
	
xthtaylor organic_fert $interest $xlist , endog($interest)
	outreg2 using Result_rc_2.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
	
	
	

	
	

**********************

xthtaylor adopt $interest $xlist, endog($interest)
	outreg2 using Result12.tex, replace ctitle (Improved seeds) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)

xthtaylor fertilisers $interest $xlist, endog($interest)
	outreg2 using Result12.tex, append ctitle (Fertilisers) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
xthtaylor pesticide $interest $xlist, endog($interest)
	outreg2 using Result12.tex, append ctitle (Pesticides) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
xthtaylor credit_cash $interest $xlist, endog($interest)
	outreg2 using Result12.tex, append ctitle (Credit) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)

	
***Result13: Hausman Taylor estimation
xthtaylor intercropping $interest $xlist, endog($interest)
	outreg2 using Result13.tex, replace ctitle (Intercropping) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
xthtaylor intercropping $interest $xlist, endog($interest)
	outreg2 using Result13.tex, append ctitle (Intercropping) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
xthtaylor crop_rotation $interest $xlist, endog($interest)
	outreg2 using Result13.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
	
xthtaylor crop_rotation $interest $xlist, endog($interest)
	outreg2 using Result13.tex, append ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, No, Year FE, Yes)
	
		
****Result14: Quantities of fertliser and pesticides
xtreg ihs_pest extension_priv extension_pub $xlist $tlist i.year i. district_1, re r
outreg2 using Result14.tex, replace ctitle (Pesticides) dec(3) lab tex(frag pr land) keep(extension_priv extension_pub $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)

	outreg2 using Result1_14, sideway stats(coef se aster  ci_low ci_high ) keep (extension_priv extension_pub)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab replace

xtreg ihs_org extension_priv extension_pub $xlist $tlist i.year i. district_1, re r
outreg2 using Result14.tex, append ctitle (Organic fertlisers) dec(3) lab tex(frag pr land) keep(extension_priv extension_pub $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) 

outreg2 using Result1_14, sideway stats(coef se aster  ci_low ci_high ) keep (extension_priv extension_pub)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab append

xtreg ihs_chem extension_priv extension_pub $xlist $tlist i.year i. district_1, re r
outreg2 using Result14.tex, append ctitle (Chemical fertilisers) dec(3) lab tex(frag pr land) keep(extension_priv extension_pub $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) 

outreg2 using Result1_14, sideway stats(coef se aster  ci_low ci_high ) keep (extension_priv extension_pub)  addstat(F test, e(F)) long  nocons  nonotes noparen dec(3) quote lab append







xtreg adopt extension_priv extension_pub $xlist $tlist i.year i. district_1, re r

margins, dydx(*) post

marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)



xtreg adopt extension_priv extension_pub $xlist $tlist i.year i. district_1, re r

coefplot, keep(extension_priv extension_pub) yline(0) 

coefplot, keep(extension_priv extension_pub) xline(0) msymbol(d) mcolor(white) levels(99 95 90 80 70) ciopts(lwidth(3 ..) lcolor(*.2 *.4 *.6 *.8 *1))  legend(order(1 "99" 2 "95" 3 "90" 4 "80" 5 "70") row(1))