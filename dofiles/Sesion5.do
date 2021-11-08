* -----------------------------------------------------------------------------
* -----------------------------------------------------------------------------
* SESIÓN 5
* CURSO STATA FUNDAUNGO
* AUTORA: ANA ESCOTO
* FECHA: 06/11/2021 - última modificación: actualicé según la clase
* -----------------------------------------------------------------------------
* -----------------------------------------------------------------------------

capture log close // cierra cualquier log abierto
set more off // Quita el boton de "more" en la ventana de resultados
clear // cierra cualquier base de datos abierta

* findit renvars

* -----------------------------------------------------------------------------
*  ESTABLECIENDO DIRECTORIOS 
* -----------------------------------------------------------------------------

gl computer "C:\Users\anaes\Dropbox\PC\Downloads\Curso_Fundaungo_STATA-main\Curso_Fundaungo_STATA-main\"
gl datos "$computer\datos"
gl temp "$computer\datos\temp"

cd "$computer\" 

log using "sesion5.log", replace


* -----------------------------------------------------------------------------
*  Importando las bases
* -----------------------------------------------------------------------------

* Cuando las bases están igual es más facil, podemos repetir comandos con loops

* Introdución a los loops

global wave 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21

foreach w of global wave {

* Importamos la base de cada año
import excel "$datos/ICI_total.xlsx", sheet("ICI 20`w'") cellrange (A7:BC50) firstrow case(lower) clear

* generamos el año que identificará nuestra ronda
gen year="20`w'", after(país)

* Despues necesitaremos esto
encode país, gen(pais2)

order pais2, after(país)

drop protección*
drop independenciadelpoderjudicial*

drop b

renvars índicede* , predrop(8) // bota los primeros 8 caracteres

renvars índice* , predrop(6) // bota los primeros 8 caracteres


save "$temp/ICI20`w'.dta", replace 

}


* -----------------------------------------------------------------------------
*  Formato ancho
* -----------------------------------------------------------------------------

use "$temp/ICI2021.dta", clear

renvars , trim(14) 

renvars  homicidios-crecimiento, postfix(_2021) // corta los nombres
save "$temp/ICI2021_wide.dta", replace


use "$temp/ICI2011.dta", clear

renvars , trim(14) 
renvars  homicidios-crecimiento, postfix(_2011) // corta los nombres
save "$temp/ICI2011_wide.dta", replace

*** Una vez identificadas  las ondas podemos hacer un merge

merge 1:1 país using  "$temp/ICI2021_wide.dta"

** No hay atrición. La atrición serían los not matched. 

/* Ahora que tenemos estos elementos podemos hacer un prueba de hipótesis 
que no habíamos hecho. 

Se trata de una prueba t, que nos dara cuenta de los cambios entre un año y otro

Esta prueba nos dará una idea de qué hacen los efectos fijos vs los aleatorios*/


ttest egini_2021==egini_2011


* -----------------------------------------------------------------------------
*  Reshape
* -----------------------------------------------------------------------------

/* Para hacer este ejercicio más claro, vamos a botar algunas variables*/

keep egini* pa* year*

reshape long egini_ participación_, i(país pais2) j(onda)


** Fijemonos en la variable year

* Queda todo con 2011. Tener cuidado

reshape wide egini_ participación_, i(país pais2) j(onda)

* -----------------------------------------------------------------------------
*  Formato long
* -----------------------------------------------------------------------------

use "$temp/ICI2007.dta", clear

global wave  08 09 10 11 12 13 14 15 16 17 18 19 20 21

foreach w of global wave {
	append using "$temp/ICI20`w'.dta"
}

describe

* Podemos ver si todas las observaciones tienen la misma cantidad de observaciones

sort pais year 
by pais: gen n=_n
by pais: gen N=_N

tab n
tab pais
tab N

/* Con este formato podemos declarar que la base es de panel*/

destring year, replace

xtset pais2 year

xtline crecimientodelpib

xtline crecimientodelpib if pais2<6, overlay

xtsum crecimientodelpib 

gen crec_dummy=crecimientodelpib>0
xttrans crec_dummy

** Heterogeneidad entre los países

bysort país: egen pib_pais=mean(crecimientodelpib)

twoway (scatter crecimientodelpib pais2, msymbol(circle_hollow)) ///
       (scatter pib_pais pais2, msymbol(diamond) )
	   
	   
** Heterogeneidad entre los años

bysort year: egen pib_y=mean(crecimientodelpib)

twoway (scatter crecimientodelpib year, msymbol(circle_hollow)) ///
       (scatter pib_y year, msymbol(diamond) )
	   

	   
* -----------------------------------------------------------------------------
*  Modelo MCO y regresión agrupada
* -----------------------------------------------------------------------------

regress crecimientodelpib escolaridadpromedio
estimates store mco

regress crecimientodelpib escolaridadpromedio i.pais2
estimates store mco_dummies

areg crecimientodelpib escolaridadpromedio, absorb(pais2)
estimates store mco_areg

esttab mco mco_dummies mco_areg, ar2 r2 se

xtreg crecimientodelpib escolaridadpromedio, fe
estimates store panel_fe

esttab mco  mco_areg panel_fe, ar2 r2 se



/*The rationale behind random effects model is that, unlike the fixed effects model, the variation across entities is assumed to be random and uncorrelated with the predictor or independent variables included in the model*/

xtreg crecimientodelpib escolaridadpromedio, re
estimates store panel_re

hausman panel_fe panel_re


* ¿Hay efectos fijos por el tiempo*

xtreg crecimientodelpib escolaridadpromedio i.year , fe
testparm i.year


*** ¿MCO o RE?
estimates restore panel_re
xttest0
* No rechazamos la H0, usamos MCO


*** Verificando heterocedasticidad

*ssc install xttest3, replace
estimates restore panel_fe
xttest3

xtreg crecimientodelpib escolaridadpromedio, fe robust
estimates store fe_robust

** Comparando.
esttab panel_fe fe_robust, scalars(rho r2_b r2_o r2) se

estimates restore panel_fe
xttest2

*cross-sectional dependence) test is used to test whether the residuals are correlated across entities*. Cross-sectional dependence can lead to bias in tests results (also called contemporaneous correlation

ssc install xtcsd, replace

xtcsd, pesaran abs
*Had cross-sectional dependence be present Hoechle suggests to use Driscoll and Kraay standard errors using the command xtscc (install it by typing ssc install xtscc). Type help xtscc for more details.

ssc install xtscc, replace

xtscc crecimientodelpib escolaridadpromedio, fe lag(2)
estimates store driscoll_kay

*** Correlación serial

*findit xtserial
xtserial crecimientodelpib escolaridadpromedio

esttab panel_fe fe_robust driscoll_kay

log close
