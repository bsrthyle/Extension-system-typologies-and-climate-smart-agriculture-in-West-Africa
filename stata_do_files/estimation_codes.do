

*===================================================================================================================================================
* Project : 	USPCALING GROUNDNUT PRODUCTIVITY
* Program:		LEARNING (TRAINING) AND GROUNDNUT PRODUCTIVITY IN GHANA, MALI AND NIGERIA
* Version 4.0 
* data: 04-12-2022  ==================================================================================================================================================
 
 *This was perfored in STATA 17
 
clear all
set maxvar 3000
set more off


****************************************************************************************************************************************************
* SET FILE PATHS
**************************************************

local path "../"


*gl data "`path'data"

*gl Results "`path'tables"


** Loading the data**
use "Groundnut", clear

****************************************************************************************************************************************************
*DATA CLEANING AND PREPARATION
****************************************************************************************************************************************************
foreach var of varlist typsoil1 typsoil2 typsoil3 nbrhoejour {
replace `var'=0 if `var' ==.
}

gen prod_value = gprod* uprice
gen sales_value = qsale* uprice


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

***Generating ordered extension variable*** I WILL CHECK THIS BEFORE RUNNING 
gen     exten = 0 if extension_access==0
replace exten=1 if exten==.& extension_priv==1 & extension_pub==0 & extension_both==0
replace exten=2 if exten==.& extension_pub==1 & extension_priv==0 & extension_both==0
replace exten=3 if exten==.& extension_both==1

label define extension 0 "No extension" 1 "Private extension" 2 "Public extension" 3 "Both Private and Public"
label values exten extension
ta exten, gen(exten)
**************************************************************************************************************************************************
*****GLOBAL VARIABLES*******

global inputs adopt organic_fert inorganic_fert fertilisers pesticide credit_cash credit_kind

global practices intercropping crop_rotation adopt organic_fert

global interest i.exten

global interest2 i.exten#training

global xlist age sexe nbschool hhsize dmurbain dmvillage association clabor_ha gsize off_farm dratio typsoil1 typsoil2 typsoil3

global tlist agebar nbschoolbar hhsizebar  gsizebar off_farmbar dratiobar

global countries Nigeria Ghana Mali

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

************************************************************************************************************************************


*****PANEL REGRESSIONS******
xtset id year
	
*** MVProbit Estimation	
*mvprobit (crop_rotation = $interest $xlist $tlist) (adopt = $interest $xlist $tlist)(intercropping = $interest $xlist $tlist)(organic_fert = $interest $xlist $tlist), difficult draws (100) vce(cluster district)

* Corrolated Randome Effect (CRE) model
* All countries

cmp (crop_rotation = $interest $xlist $tlist  i.year i. district_1) (adopt = $interest $xlist $tlist i.year i. district_1)(intercropping = $interest $xlist $tlist i.year i. district_1)(organic_fert = $interest $xlist $tlist i.year i. district_1),indicators("$cmp_probit" "$cmp_probit" "$cmp_probit" $cmp_probit) nolrtest difficult nonrtolerance vce(cluster district)
outreg2 using si_mvp1.tex, keep($interest $xlist $tlist) lab  addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) replace

margins, dydx(exten) predict(eq(#1) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels replace


margins, dydx(exten) predict(eq(#2) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge


margins, dydx(exten) predict(eq(#3) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge


margins, dydx(exten) predict(eq(#4) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge

* without control 
cmp (crop_rotation = $interest i.year) (adopt = $interest i.year )(intercropping = $interest  i.year )(organic_fert = $interest i.year ),indicators("$cmp_probit" "$cmp_probit" "$cmp_probit" $cmp_probit) nolrtest difficult nonrtolerance vce(cluster district)
outreg2 using si_mvp1_1.tex, keep($interest $xlist $tlist) lab  addtext(Additional controls, No, District FE, No, Year FE, Yes) replace


* GHANA

cmp (crop_rotation = $interest $xlist $tlist  i.year i. district_1) (adopt = $interest $xlist $tlist i.year i. district_1)(intercropping = $interest $xlist $tlist i.year i. district_1)(organic_fert = $interest $xlist $tlist i.year i. district_1) if country=="Ghana",indicators("$cmp_probit" "$cmp_probit" "$cmp_probit" $cmp_probit) nolrtest difficult nonrtolerance vce(cluster district)
outreg2 using si_mvp2.tex, keep($interest $xlist $tlist) lab  addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) replace

margins, dydx(exten) predict(eq(#1) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge


margins, dydx(exten) predict(eq(#2) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge


margins, dydx(exten) predict(eq(#3) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge


margins, dydx(exten) predict(eq(#4) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge



* MALI
cmp (crop_rotation = $interest $xlist $tlist  i.year i. district_1) (adopt = $interest $xlist $tlist i.year i. district_1)(intercropping = $interest $xlist $tlist i.year i. district_1)(organic_fert = $interest $xlist $tlist i.year i. district_1) if country=="Mali",indicators("$cmp_probit" "$cmp_probit" "$cmp_probit" $cmp_probit) nolrtest difficult nonrtolerance vce(cluster district)
outreg2 using si_mvp3.tex, keep($interest $xlist $tlist) lab  addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) replace

margins, dydx(exten) predict(eq(#1) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge


margins, dydx(exten) predict(eq(#2) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge


margins, dydx(exten) predict(eq(#3) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge


margins, dydx(exten) predict(eq(#4) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge
	

* NIGERIA

cmp (crop_rotation = $interest $xlist $tlist  i.year i. district_1) (adopt = $interest $xlist $tlist i.year i. district_1)(intercropping = $interest $xlist $tlist i.year i. district_1)(organic_fert = $interest $xlist $tlist i.year i. district_1) if country=="Nigeria",indicators("$cmp_probit" "$cmp_probit" "$cmp_probit" $cmp_probit) nolrtest difficult nonrtolerance vce(cluster district)
outreg2 using si_mvp4.tex, keep($interest $xlist $tlist) lab  addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) replace

margins, dydx(exten) predict(eq(#1) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge


margins, dydx(exten) predict(eq(#2) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge


margins, dydx(exten) predict(eq(#3) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge


margins, dydx(exten) predict(eq(#4) pr) force noestimcheck
outreg using marginal_effects.txt, stat(b_dfdx se_dfdx)  varlabels merge



/******************Robustness check************************/


***OSTER BOUNDS***
ssc install psacalc

/*Crop rotation*/
xtreg crop_rotation exten2 exten3 exten4 $xlist $tlist, fe vce(cluster id)
psacalc beta exten2, rmax(0.078)
psacalc beta exten3, rmax(0.078)
psacalc beta exten4, rmax(0.078)

psacalc delta exten2, rmax(0.078)
psacalc delta exten3, rmax(0.078)
psacalc delta exten4, rmax(0.078)


/*Adoption*/
xtreg adopt exten2 exten3 exten4 $xlist $tlist, fe vce(cluster id)
psacalc beta exten2, rmax(0.075)
psacalc beta exten3, rmax(0.075)
psacalc beta exten4, rmax(0.075)

psacalc delta exten2, rmax(0.075)
psacalc delta exten3, rmax(0.075)
psacalc delta exten4, rmax(0.075)



/*Intercropping*/
xtreg intercropping exten2 exten3 exten4 $xlist $tlist, fe vce(cluster id)
psacalc beta exten2, rmax(0.073)
psacalc beta exten3, rmax(0.073)
psacalc beta exten4, rmax(0.073)

psacalc delta exten2, rmax(0.073)
psacalc delta exten3, rmax(0.073)
psacalc delta exten4, rmax(0.073)


/*Organic fertilizer*/
xtreg organic_fert exten2 exten3 exten4 $xlist $tlist, fe vce(cluster id)
psacalc beta exten2, rmax(0.11)
psacalc beta exten3, rmax(0.11)
psacalc beta exten4, rmax(0.11)

psacalc delta exten2, rmax(0.11)
psacalc delta exten3, rmax(0.11)
psacalc delta exten4, rmax(0.11)




*** Hausman Taylor estimation***

xi: xthtaylor crop_rotation $interest $xlist i.year i.district_1  , endog($interest) vce(r)
xi: outreg2 using Hausman.tex, replace ctitle (Crop Rotation) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)

xi: xthtaylor adopt $interest $xlist i.year i.district_1  , endog($interest) vce(r)
xi:outreg2 using Hausman.tex, append ctitle (Improved Seeds) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes) 
	
xi: xthtaylor intercropping $interest $xlist i.year i.district_1 , endog($interest) vce(r)
xi: outreg2 using Hausman.tex, append ctitle (Intercropping) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)
		
xi: xthtaylor organic_fert $interest $xlist i.year i.district_1 , endog($interest) vce(r)
xi: outreg2 using Hausman.tex, append ctitle (Organic Fertilisers) dec(3) lab tex(frag pr land) keep($interest $xlist) addtext(Additional controls, Yes, District FE, Yes, Year FE, Yes)





***Imben's sensitivity check (Imbens, G. W. (2003) / Sensitivity to exogeneity assumptions in program evaluation. American Economic Review, 93(2), 126-132)

tesensitivity cpi (crop_rotation $xlist $tlist i.year i. district_1) (exten2 $xlist $tlist i.year i. district_1), ate
estimates store exten2
tesensitivity cpi (crop_rotation $xlist $tlist i.year i. district_1) (exten3 $xlist $tlist i.year i. district_1), ate 
estimates store exten3 
tesensitivity cpi (crop_rotation $xlist $tlist i.year i. district_1) (exten4 $xlist $tlist i.year i. district_1), ate 
estimates store exten4

tesensitivity cpitable exten2 exten3 exten4

tesensitivity cpi (adopt $xlist $tlist i.year i. district_1) (exten2 $xlist $tlist i.year i. district_1), ate
tesensitivity cpi (adopt $xlist $tlist i.year i. district_1) (exten3 $xlist $tlist i.year i. district_1), ate 
tesensitivity cpi (adopt $xlist $tlist i.year i. district_1) (exten4 $xlist $tlist i.year i. district_1), ate 

tesensitivity cpi (intercropping $xlist $tlist i.year i. district_1) (exten2 $xlist $tlist i.year i. district_1), ate
tesensitivity cpi (intercropping $xlist $tlist i.year i. district_1) (exten3 $xlist $tlist i.year i. district_1), ate  
tesensitivity cpi (intercropping $xlist $tlist i.year i. district_1) (exten4 $xlist $tlist i.year i. district_1), ate 

tesensitivity cpi (organic_fert $xlist $tlist i.year i. district_1) (exten2 $xlist $tlist i.year i. district_1), ate
tesensitivity cpi (organic_fert $xlist $tlist i.year i. district_1) (exten3 $xlist $tlist i.year i. district_1), ate  
tesensitivity cpi (organic_fert $xlist $tlist i.year i. district_1) (exten4 $xlist $tlist i.year i. district_1), ate 



/******************************Breackdown******************************/
*regression breakdown calculates the maximum value of a sensitivity parameter under which a given hypothesis holds for all values of the regression coefficients in the identified set.

/*Private extension*/
local y10 crop_rotation
local y11 adopt
local y12 intercropping
local y13 organic_fert

local x exten2
local w1 $xlist 
local w0 $tlist
local w `w1' `w0'
local cluster (district)

regsensitivity breakdown `y10' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) plot
regsensitivity bounds `y10' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) 
regsensitivity plot, title("Crop Rotation") boundcolors ("red")
graph save "bd_privatext_crop.gph", replace


regsensitivity breakdown `y11' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) plot
regsensitivity bounds `y11' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) 
regsensitivity plot, title("Improved seeds") boundcolors ("red")
graph save "bd_privatext_adopt.gph", replace

regsensitivity breakdown `y12' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) plot
regsensitivity bounds `y12' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) 
regsensitivity plot, title("Intercropping") boundcolors ("red")
graph save "bd_privatext_interc.gph", replace

regsensitivity breakdown `y13' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) plot
regsensitivity bounds `y13' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) 
regsensitivity plot, title("Organic Fertilisers") boundcolors ("red")
graph save "bd_privatext_org.gph", replace

graph combine bd_privatext_crop.gph bd_privatext_adopt.gph bd_privatext_interc.gph bd_privatext_org.gph, cols(2) title("Private extension")
graph save combined_private.gph, replace
graph export combined_private.png, replace


/*Public extension*/
local y10 crop_rotation
local y11 adopt
local y12 intercropping
local y13 organic_fert

local x exten3
local w1 $xlist 
local w0 $tlist
local w `w1' `w0'
local cluster (district)

regsensitivity breakdown `y10' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) plot
regsensitivity bounds `y10' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) 
regsensitivity plot, title("Crop Rotation") boundcolors ("red")

graph save "bd_publicext_crop.gph", replace

regsensitivity breakdown `y11' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) plot
regsensitivity bounds `y11' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) 
regsensitivity plot, title("Improved seeds") boundcolors ("red")

graph save "bd_publicext_adopt.gph", replace

regsensitivity breakdown `y12' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) plot
regsensitivity bounds `y12' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) 
regsensitivity plot, title("Intercropping") boundcolors ("red")

graph save "bd_publicext_interc.gph", replace

regsensitivity breakdown `y13' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) plot
regsensitivity bounds `y13' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) 
regsensitivity plot, title("Organic Fertilisers") boundcolors ("red")

graph save "bd_publicext_org.gph", replace

graph combine bd_publicext_crop.gph bd_publicext_adopt.gph bd_publicext_interc.gph bd_publicext_org.gph, cols(2) title("Public extension")
graph save combined_public.gph, replace
graph export combined_public.png, replace

/*Both public and private extension*/
local y10 crop_rotation
local y11 adopt
local y12 intercropping
local y13 organic_fert

local x exten4
local w1 $xlist 
local w0 $tlist
local w `w1' `w0'
local cluster (district)

regsensitivity breakdown `y10' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) plot
regsensitivity bounds `y10' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) 
regsensitivity plot, title("Crop Rotation") boundcolors ("red")

graph save "bd_bothext_crop.gph", replace

regsensitivity breakdown `y11' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) plot
regsensitivity bounds `y11' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) 
regsensitivity plot, title("Improved seeds") boundcolors ("red")

graph save "bd_bothext_adopt.gph", replace

regsensitivity breakdown `y12' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) plot
regsensitivity bounds `y12' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) 
regsensitivity plot, title("Intercropping") boundcolors ("red")

graph save "bd_bothext_interc.gph", replace

regsensitivity breakdown `y13' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) plot
regsensitivity bounds `y13' `x' `w', compare(`w') cbar (0(.2)1) rxbar(0(.2)2) 
regsensitivity plot, title("Organic Fertilisers") boundcolors ("red")
graph save "bd_bothext_org.gph", replace

graph combine bd_bothext_crop.gph bd_bothext_adopt.gph bd_bothext_interc.gph bd_bothext_org.gph, cols(2) title("Joint Public-private extension")
graph save combined_joint.gph, replace
graph export combined_joint.png, replace
