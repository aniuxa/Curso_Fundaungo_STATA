* -----------------------------------------------------------------------------
* -----------------------------------------------------------------------------
* SESIÓN 4
* CURSO STATA FUNDAUNGO
* AUTORA: ANA ESCOTO
* FECHA:30/10/2021 - última modificación: actualicé según la clase
* -----------------------------------------------------------------------------
* -----------------------------------------------------------------------------

capture log close // cierra cualquier log abierto
set more off // Quita el boton de "more" en la ventana de resultados
clear // cierra cualquier base de datos abierta


* -----------------------------------------------------------------------------
*  ESTABLECIENDO DIRECTORIOS Y ABRIENDO BASE DE DATOS
* -----------------------------------------------------------------------------

global computer "C:\Users\anaes\Dropbox\PC\Downloads\"

cd "$computer\Curso_Fundaungo_STATA-main\Curso_Fundaungo_STATA-main\" 

log using "sesion4.log", replace

use "datos\ehpm_2019.dta", clear


* -----------------------------------------------------------------------------
*  Extensiones de la regresión: efectos cuadráticos
* -----------------------------------------------------------------------------

gen log_money=log(money)


regress log_money aproba1 i.r104 ib5.region r106 if actpr2012==10
estimates store modelo4, title("Lineal")


** Gráficamente

graph bar (mean) money if actpr2012==10, over(aproba1)
graph bar (mean) log_money if actpr2012==10, over(aproba1)

gen aproba2=aproba1^2


scatter log_money aproba2, jitter(50) mcolor(%20)

pwcorr log_money aproba* if actpr2012==10, sig obs

regress log_money aproba* i.r104 ib5.region r106 if actpr2012==10
estimates store modelo5, title("Cuadrático")

ftest modelo4 modelo5

estimates restore modelo5
margins, dydx(aproba*)


regress log_money c.aproba1##c.aproba1 i.r104 ib5.region r106 if actpr2012==10
estimates store modelo6, title("Cuadrático2")

estimates restore modelo6
margins, dydx(aproba*)

scatter log_money r106, jitter(50) mcolor(%20)
graph bar (mean) log_money, over(r106)


* -----------------------------------------------------------------------------
*  Extensiones de la regresión: interacciones
* -----------------------------------------------------------------------------

regress log_money c.aproba1##i.r104 ib5.region r106 if actpr2012==10
estimates store modelo7, title("EscolaridadXsexo")

** Marginales para el modelo sin interacciones, a varios niveles de escolaridad

estimates restore modelo4
margins r104, at(aproba1=(0(4)25))
marginsplot

estimates restore modelo7
margins r104, at(aproba1=(0(4)25))
marginsplot

regress log_money c.aproba1##c.r106 i.r104 ib5.region  if actpr2012==10

regress log_money aproba1 i.r104##ib5.region r106 if actpr2012==10
estimates store modelo8, title("SexoXregion")

* -----------------------------------------------------------------------------
* Uso de lincom y nlcom
* -----------------------------------------------------------------------------

estimates replay modelo8, coeflegend

lincom  _b[2.r104] + _b[4.region] +  _b[2.r104#4.region]

nlcom _b[2.r104]/_b[4.region]


* Con diseño muestral

svyset correlativo [pw=fac00],  ///
strata(estratoarea) vce(linearized) singleunit(certainty)

svy: mean  money if actpr2012==10, over(r104)
svy: mean  money if actpr2012==10, over(r104) coeflegend
lincom _b[c.money@1bn.r104]- _b[c.money@2.r104]


* -----------------------------------------------------------------------------
*  Otras regresiones
* -----------------------------------------------------------------------------

** Robusta a los datos atípicos

rreg log_money  aproba1 r106 i.r104 ib5.region if actpr2012==10
estimates store modelo9
di"R2 = " e(r2)
di "R2a = " e(r2_a)

rregfit 

rreg log_money  aproba1 r106 i.r104 ib5.region i.area if actpr2012==10
rregfit

** Regresión cuantílica o mediana

qreg log_money  aproba1 r106 i.r104 ib5.region if actpr2012==10
estimates store modelo10

*bsqreg log_money  aproba1 r106 i.r104 ib5.region if actpr2012==10, reps(1000)
*estimates store modelo_lyla

esttab modelo*

* -----------------------------------------------------------------------------
* Modelo logístico
* -----------------------------------------------------------------------------

gen pobre_total=pobreza<3

logit pobre_total aproba1 i.r104 ib5.region r106 if r103==1
estimates store logit1

estimates replay logit1, or


logit pobre_total aproba1 i.r104 ib5.region r106 if r103==1
fitstat
estadd fitstat
estimates store logit1


logit pobre_total aproba1 i.r104 ib5.region r106 i.area if r103==1
estadd fitstat
estimates store logit2

esttab logit* using "out/resultados_logit.rtf", eform cells(b(fmt(2) star)) ///
scalars(r2_mf r2_mfadj r2_ml r2_ct dev ll bic aic) ///
starlevel(* 0.10 ** 0.05 *** 0.001) ///
legend label replace unstack  

** Multicolinealidad

* Hay que instalar collin 
* findit collin 

collin aproba1 r104 region r106 area

* Revisar valores atípicos
predict p
predict stdres, rstand
scatter stdres p

** Post-estimación

estimates restore logit2

margins

margins r104, at(aproba=(0(5)25))

estimates restore logit2
margins, dydx(*) post
estimates store eff2

coefplot eff2, xline(0) scheme(plottig)

log close
