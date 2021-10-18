* -----------------------------------------------------------------------------
* -----------------------------------------------------------------------------
* SESIÓN 2
* CURSO STATA FUNDAUNGO
* AUTORA: ANA ESCOTO
* FECHA: 16/10/2021 - última modificación: actualicé según la clase
* -----------------------------------------------------------------------------
* -----------------------------------------------------------------------------

capture log close // cierra cualquier log abierto
set more off // Quita el boton de "more" en la ventana de resultados
clear // cierra cualquier base de datos abierta


* -----------------------------------------------------------------------------
*  ESTABLECIENDO DIRECTORIOS Y ABRIENDO BASE DE DATOS
* -----------------------------------------------------------------------------

cd "C:\Users\anaes\Dropbox\PC\Downloads\Curso_Fundaungo_STATA-main\Curso_Fundaungo_STATA-main\" 

log using "sesion2.log", replace

use "datos\ehpm_2019.dta", clear


* -----------------------------------------------------------------------------
*  CONTINUACIÓN DE GRÁFICOS 
* -----------------------------------------------------------------------------

** GRÁFICOS DE PASTEL
graph pie if actpr2012==10 , over(r104)

graph pie if actpr2012==10 , over(r104) plabel(_all percent)

*** GRÁFICOS DE BARRA

ssc install catplot, replace

catplot r104 if actpr2012==10

catplot r104 actpr2012 if r106>15, percent(r104)


catplot r104 actpr2012 if r106>15, ///
percent(r104) ///
var1opts(label(labsize(medium))) ///
var2opts(label(labsize(medium)))  ///
title("Condición de actividad" ///
, span size(medium)) ///
blabel(bar, format(%4.1f)) ///
intensity(25) ///
asyvars

graph save "graphs\mi_primergrafico.gph", replace
graph export "graphs\mi_primergrafico.png", replace width(5000)


 graph query, schemes
 
 ssc install blindschemes, replace

 
catplot r104 actpr2012 if r106>15, ///
percent(r104) ///
var1opts(label(labsize(medium))) ///
var2opts(label(labsize(medium)))  ///
title("Condición de actividad" ///
, span size(medium)) ///
blabel(bar, format(%4.1f)) ///
intensity(25) ///
asyvars scheme(plotplainblind)


* -----------------------------------------------------------------------------
*  INFERENCIA
* -----------------------------------------------------------------------------

** Intervalos de confianza

ci means money if actpr2012==10

ci means money if actpr2012==10, level(99)

ci means r01b, poisson // para una variable de conteo tipo Poisson

tab r104, gen(s_)
gen dummy_mujer=(r104==2)
ci proportion s_1 s_2


*** Pruebas de hipótesis

** Para medias
ttest money==240 if actpr2012==10

ttest money if actpr2012==10, by(r104)

ttest money if actpr2012==10, by(r104) unequal

* Varianzas


sdtest money==10 if actpr2012==10

sdtest money if actpr2012==10, by(r104)

* Prueba chi-cuadrado

tab actpr2012 r104 if r106>15, chi

tab actpr2012 r104 if r106>15, expected
tab actpr2012 r104 if r106>15, cchi


** ANOVA
oneway money region if actpr2012==10

*** Pruebas no paramétricas


* Para diferencias de dos grupos

ranksum money if actpr2012==10, by(r104)

* Para diferencias de más de dos grupos

kwallis money if actpr2012==10, by(region)


*******************************************************************************
* CORRELACIONES
*******************************************************************************

graph matrix money r106 aproba1 if actpr2012==10


corr  money r106 aproba1 if actpr2012==10

pwcorr  money r106 aproba1 if actpr2012==10

pwcorr money r106 aproba1 if actpr2012==10, sig obs star(0.05)

spearman money r106 aproba1 if actpr2012==10, stats(rho p)

ktau money r106 aproba1 if actpr2012==10, stats(taua p)



*******************************************************************************
* DISEÑO MUESTRAL COMPLEJO
*******************************************************************************

* Primero se necesita establecer el diseño

svyset correlativo [pw=fac00],  ///
strata(estratoarea) vce(linearized) singleunit(certainty)

mean ingfa if r103==1 

mean ingfa if r103==1 [pw=fac00]

svy: mean ingfa if r103==1
estat cv


svy: mean ingfa if r103==1, over(region r104)
estat cv



svy: mean ingpe, over(region r104)
estat cv


svy: proportion actpr2012  if r106>15
estat cv


svy: proportion actpr2012  if r106>15, over(region r104)
estat cv

log close
