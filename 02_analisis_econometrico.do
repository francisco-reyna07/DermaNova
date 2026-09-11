* Leer Base de Datos Final

clear all
set more off

use "https://github.com/francisco-reyna07/DermaNova/raw/refs/heads/main/base_final.dta", clear

label variable Geography       "País"
label variable Category        "Categoría de producto"
label variable CompanyName     "Nombre de la empresa"
label variable DataType        "Tipo de dato"
label variable Year            "Año"
label variable Company_Share   "Participación de mercado de la empresa (%)"
label variable Num_Brands      "Número de marcas de la empresa"
label variable Num_New_Brands  "Número de marcas nuevas"
label variable New_Brand       "Indicador de nueva marca"
label variable Market_Size     "Tamaño del mercado"
label variable Estimated_Sales "Ventas estimadas de la empresa"
label variable Sales_Change    "Cambio en ventas"
label variable Market_Growth   "Crecimiento del mercado (%)"

describe



****************************
****************************
***                      ***
*** Análisis Descriptivo ***
***                      ***
****************************
****************************

tabstat Year Company_Share Num_Brands Num_New_Brands New_Brand ///
        Market_Size Estimated_Sales Sales_Change Market_Growth, ///
        stat(n mean sd min p50 max) col(stat) format(%12.2f)
		
* EDA - Distribución de los datos

tab Year

tab Category, sort

* Total de nuevas marcas por año

table Year, statistic(sum Num_New_Brands)

* Nuevas marcas por año

tab Year New_Brand, row

* Características según entrada de nueva marca

table New_Brand, ///
    statistic(mean Company_Share) ///
    statistic(mean Num_Brands) ///
    statistic(mean Estimated_Sales) ///
    statistic(mean Market_Growth)

* Evolución de nuevas marcas

preserve

collapse (sum) Num_New_Brands, by(Year)

graph bar Num_New_Brands, over(Year) ///
    title("Número total de nuevas marcas por año") ///
    ytitle("Número de nuevas marcas")

restore

* Porcentaje de observaciones con nuevas marcas

preserve

collapse (mean) New_Brand, by(Year)

replace New_Brand = New_Brand * 100

twoway connected New_Brand Year, ///
    title("Presencia de nuevas marcas por año") ///
    xtitle("Año") ///
    ytitle("Observaciones con nueva marca (%)")

restore

* 1. Niveles, diferencias y tendencias

* Distribución de la participación de mercado
summarize Company_Share, detail

* Participación de mercado por año
tabstat Company_Share, by(Year) stat(mean sd min max)

* Participación de mercado por categoría
tabstat Company_Share, by(Category) stat(mean sd min max)

* Participación de mercado según entrada de nueva marca
tabstat Company_Share, by(New_Brand) stat(n mean sd min max)

* Tendencia de participación de mercado según entrada de nueva marca

preserve

collapse (mean) Company_Share, by(Year New_Brand)

twoway ///
    (line Company_Share Year if New_Brand==0, sort) ///
    (line Company_Share Year if New_Brand==1, sort), ///
    title("Participación de mercado según entrada de nuevas marcas") ///
    xtitle("Año") ///
    ytitle("Participación de mercado promedio (%)") ///
    legend(order(1 "Sin nueva marca" 2 "Con nueva marca"))

restore

****************************
****************************
***                      ***
***     Regresiones      ***
***     Exploratorias    ***
***                      ***
****************************
****************************

* 1. Tendencia y regresión pooled

gen trend = Year - 2016

summarize trend

reg Company_Share trend

* 2. Comparar compañias con dummy

tab New_Brand

reg Company_Share c.trend##i.New_Brand

margins New_Brand, at(trend=(0(1)9))

marginsplot, ///
    title("Participación de mercado según entrada de nueva marca") ///
    xtitle("Tendencia (0 = 2016)") ///
    ytitle("Participación de mercado predicha (%)") ///
    legend(order(1 "Sin nueva marca" 2 "Con nueva marca"))

* 3. Controles y especificaciones sencillas

encode Category, gen(Category_ID)

tab Category_ID

* Definir controles
global controls Market_Growth Market_Size

* Modelo sin controles
reg Company_Share New_Brand

* Modelo con controles
reg Company_Share New_Brand $controls

* Modelo con controles y categoría
reg Company_Share New_Brand $controls i.Category_ID

* Modelo con controles, categoría y año
reg Company_Share New_Brand $controls i.Category_ID i.Year

* 4. Crecimiento vs. riesgo

* Participación de mercado según entrada de nueva marca
tabstat Company_Share, by(New_Brand) stat(mean sd min max)

* Participación de mercado por categoría
tabstat Company_Share, by(Category) stat(mean sd min max)

* 5. Ventas estimadas y entrada de nueva marca

tabstat Estimated_Sales, by(New_Brand) stat(n mean sd min max)

preserve

collapse (mean) Estimated_Sales, by(Year New_Brand)

twoway ///
    (line Estimated_Sales Year if New_Brand==0, sort) ///
    (line Estimated_Sales Year if New_Brand==1, sort), ///
    title("Ventas estimadas según entrada de nueva marca") ///
    xtitle("Año") ///
    ytitle("Ventas estimadas promedio") ///
    legend(order(1 "Sin nueva marca" 2 "Con nueva marca"))

restore

reg Estimated_Sales New_Brand

reg Estimated_Sales c.trend##i.New_Brand

margins New_Brand, at(trend=(1(1)9))

marginsplot, ///
    title("Ventas estimadas según entrada de nueva marca") ///
    xtitle("Tendencia (1 = 2017)") ///
    ytitle("Ventas estimadas predichas") ///
    legend(order(1 "Sin nueva marca" 2 "Con nueva marca"))
	
reg Estimated_Sales New_Brand Market_Growth ///
    i.Category_ID i.Year
	
* Tendencia con controles

reg Estimated_Sales c.trend##i.New_Brand ///
    Market_Growth i.Category_ID
	
* 7. Regresión de panel con efectos fijos

* Crear identificador del panel
egen panel_id = group(CompanyName Category)

* Declarar estructura de panel
xtset panel_id Year

* Outcome 1
xtreg Company_Share New_Brand Market_Growth Market_Size ///
    i.Year, fe vce(cluster panel_id)

* Outcome 2
xtreg Estimated_Sales New_Brand Market_Growth ///
    i.Year, fe vce(cluster panel_id)
	
****************************
****************************
***                      ***
***       Análisis       ***
***     Econométrico     ***
***      Preliminar      ***
***                      ***
****************************
****************************


/*

*** PSM - Participación de mercado ***

* Variables previas al tratamiento

gen L_Num_Brands = L.Num_Brands
gen L_Company_Share = L.Company_Share
gen L_Estimated_Sales = L.Estimated_Sales

* Revisar tratamiento y variables

tab New_Brand

summarize Company_Share L_Num_Brands L_Company_Share L_Estimated_Sales

* Propensity Score Matching

psmatch2 New_Brand L_Num_Brands L_Company_Share L_Estimated_Sales, ///
    outcome(Company_Share) neighbor(1) common

* Prueba de balance

pstest L_Num_Brands L_Company_Share L_Estimated_Sales, both

*/

*** DIferencias Simples ***

* 1. Diferencias simples entre grupos

tabstat Company_Share, by(New_Brand) statistics(mean sd n)

reg Company_Share New_Brand

tabstat Estimated_Sales, by(New_Brand) statistics(mean sd n)

reg Estimated_Sales New_Brand

* 2. Crear periodo antes y después

bysort panel_id: egen first_treat = min(cond(New_Brand==1, Year, .))

gen ever_treated = first_treat < .

gen post = Year >= first_treat if ever_treated == 1

tab post

* Participación de mercado antes y después

tabstat Company_Share if ever_treated==1, by(post) statistics(mean sd n)

reg Company_Share post if ever_treated==1

* Ventas estimadas antes y después

tabstat Estimated_Sales if ever_treated==1, by(post) statistics(mean sd n)

reg Estimated_Sales post if ever_treated==1



****************************
****************************
***                      ***
***       Análisis       ***
***     Econométrico     ***
***       Principal      ***
***                      ***
****************************
****************************

*** 1. DIFERENCIAS EN DIFERENCIAS ESCALONADO ***

* Cohorte de tratamiento
capture drop gvar
gen gvar = first_treat
replace gvar = 0 if missing(gvar)

* Revisar cohortes
capture drop tag_panel
bysort panel_id (Year): gen tag_panel = (_n == 1)

tab gvar if tag_panel

* Participación de mercado
csdid Company_Share, ///
    ivar(panel_id) ///
    time(Year) ///
    gvar(gvar) ///
    notyet

estat simple
estat event
csdid_plot

* Ventas estimadas
csdid Estimated_Sales, ///
    ivar(panel_id) ///
    time(Year) ///
    gvar(gvar) ///
    notyet

estat simple
estat event
csdid_plot

* Participación de mercado
csdid Company_Share, ///
    ivar(panel_id) ///
    time(Year) ///
    gvar(gvar) ///
    notyet

estat pretrend

* Ventas estimadas
csdid Estimated_Sales, ///
    ivar(panel_id) ///
    time(Year) ///
    gvar(gvar) ///
    notyet

estat pretrend



*** SYNTHETIC DIFFERENCE IN DIFFERENCES ***

* Identificar paneles completos

capture drop n_share n_sales

bysort panel_id: egen n_share = total(!missing(Company_Share))
bysort panel_id: egen n_sales = total(!missing(Estimated_Sales))

preserve

* SDID requiere panel balanceado
keep if n_share == 10 & n_sales == 10

* Tratamiento absorbente desde la primera entrada
capture drop treated_sdid

gen treated_sdid = 0
replace treated_sdid = 1 if ///
    first_treat < . & Year >= first_treat

* Revisar tratamiento
tab Year treated_sdid


* SDID - Participación de mercado

sdid Company_Share panel_id Year treated_sdid, ///
    vce(placebo) ///
    reps(200) ///
    seed(50) ///
    graph


* SDID - Ventas estimadas

sdid Estimated_Sales panel_id Year treated_sdid, ///
    vce(placebo) ///
    reps(200) ///
    seed(50) ///
    graph

restore

*** EVENT STUDY SDID ***

capture drop n_share n_sales

bysort panel_id: egen n_share = total(!missing(Company_Share))
bysort panel_id: egen n_sales = total(!missing(Estimated_Sales))

preserve

keep if n_share == 10 & n_sales == 10

capture drop treated_sdid

gen treated_sdid = 0
replace treated_sdid = 1 if first_treat < . & Year >= first_treat


* Participación de mercado

sdid_event Company_Share ///
    panel_id Year treated_sdid, ///
    vce(placebo) ///
    brep(200) ///
    placebo(all)

* Gráfica Event Study - Participación

matrix res = e(H)[2..16,1..5]

svmat res

gen event_time = _n - 1 if !missing(res1)

* Effect 1 = t0; Placebo 1 = t-1
replace event_time = 9 - _n if _n > 9 & !missing(res1)

sort event_time

twoway ///
    (rcap res3 res4 event_time) ///
    (scatter res1 event_time), ///
    yline(0, lpattern(dash)) ///
    xline(0, lpattern(dash)) ///
    xlabel(-6(1)8) ///
    xtitle("Años relativos a la entrada de la nueva marca") ///
    ytitle("Efecto sobre participación (pp)") ///
    title("Efecto dinámico sobre participación de mercado") ///
    legend(off)

graph export "SDID_Event_Participacion.png", replace

drop res1 res2 res3 res4 res5 event_time


* Ventas estimadas

sdid_event Estimated_Sales ///
    panel_id Year treated_sdid, ///
    vce(placebo) ///
    brep(200) ///
    placebo(all)

* Gráfica Event Study - Ventas

matrix res = e(H)[2..16,1..5]

svmat res

gen event_time = _n - 1 if !missing(res1)

* Effect 1 = t0; Placebo 1 = t-1
replace event_time = 9 - _n if _n > 9 & !missing(res1)

sort event_time

twoway ///
    (rcap res3 res4 event_time) ///
    (scatter res1 event_time), ///
    yline(0, lpattern(dash)) ///
    xline(0, lpattern(dash)) ///
    xlabel(-6(1)8) ///
    xtitle("Años relativos a la entrada de la nueva marca") ///
    ytitle("Efecto sobre ventas estimadas") ///
    title("Efecto dinámico sobre ventas estimadas") ///
    legend(off)

graph export "SDID_Event_Ventas.png", replace

drop res1 res2 res3 res4 res5 event_time

restore



*** 3. PSM + DID ESCALONADO ***

* Guardar base completa
tempfile base_completa matched_ids
save `base_completa'

* Línea base: 2016
keep if Year == 2016

* Mantener observaciones completas
drop if missing(Num_Brands, Company_Share, Estimated_Sales)

* Transformación de ventas
gen ln_Estimated_Sales = ln(Estimated_Sales + 1)

* Estimar propensity score
logit ever_treated ///
    Num_Brands ///
    Company_Share ///
    ln_Estimated_Sales

predict pscore, pr

* Caliper = 0.2 desviaciones estándar del log-odds
gen logit_pscore = ln(pscore/(1-pscore))

summarize logit_pscore
local caliper = .2*r(sd)

* Matching exacto por categoría
gen matched_cat = 0

set seed 50
gen orden_psm = runiform()

levelsof Category_ID, local(categorias)

foreach c of local categorias {

    count if Category_ID == `c' & ever_treated == 1
    local tratados = r(N)

    count if Category_ID == `c' & ever_treated == 0
    local controles = r(N)

    if `tratados' > 0 & `controles' > 0 {

        capture drop _treated _support _weight _id _n1 _nn _pdif

        sort Category_ID orden_psm

        psmatch2 ever_treated if Category_ID == `c', ///
            pscore(pscore) ///
            odds ///
            neighbor(1) ///
            common ///
            noreplacement ///
            caliper(`caliper')

        replace matched_cat = 1 if ///
            Category_ID == `c' & ///
            ((_treated == 1 & _support == 1) | ///
             (_treated == 0 & _weight == 1))
    }
}

* Guardar paneles emparejados
keep if matched_cat == 1

keep panel_id
duplicates drop panel_id, force

save `matched_ids'

* Recuperar panel completo
use `base_completa', clear

merge m:1 panel_id using `matched_ids'

keep if _merge == 3
drop _merge

* Revisar muestra emparejada
capture drop tag_matched
bysort panel_id (Year): gen tag_matched = (_n == 1)

tab ever_treated if tag_matched
tab gvar if tag_matched


* DID - Participación de mercado

csdid Company_Share, ///
    ivar(panel_id) ///
    time(Year) ///
    gvar(gvar) ///
    notyet

estat simple
estat event
estat pretrend


* DID - Ventas estimadas

csdid Estimated_Sales, ///
    ivar(panel_id) ///
    time(Year) ///
    gvar(gvar) ///
    notyet

estat simple
estat event
estat pretrend