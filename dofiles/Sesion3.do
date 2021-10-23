 -----------------------------------------------------------------------------
* -----------------------------------------------------------------------------
* SESIÓN 3
* CURSO STATA FUNDAUNGO
* AUTORA: ANA ESCOTO
* FECHA:23/10/2021 - última modificación: actualicé según la clase
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

log using "sesion3.log", replace

use "datos\ehpm_2019.dta", clear


* -----------------------------------------------------------------------------
*  Regresión lineal
* -----------------------------------------------------------------------------

* Nuestra variable dependiente

gladder money if actpr2012==10 & money>0 // histogramas según escalera
qladder money if actpr2012==10 & money>0 // qqplots según escalera
ladder money if actpr2012==10 & money>0 // pruebas de hipótesis.

gen log_money=log(money) if actpr2012==10 & money>0

* La versión gráfica 

twoway scatter log_money aproba1 if actpr2012==10, jitter(50) mcolor(%20)


graph bar (mean) money if actpr2012==10, over(aproba1)

graph box money if actpr2012==10, over(aproba1)

** ¿Cómo se vería una línea recta?

twoway lfit log_money aproba1 if actpr2012==10

twoway (scatter log_money aproba1 if actpr2012==10, jitter(50) mcolor(%20)) ///
       (lfit log_money aproba1 if actpr2012==10)
	   
	   
* Regresión lineal

regress log_money aproba1 if actpr2012==10

regress log_money aproba1 if actpr2012==10, level(99)

regress log_money aproba1 if actpr2012==10, beta

ssc install estout, replace // para tablas de estimaciones
ssc install coefplot, replace // graficos de coeficientes

esttab
coefplot

estimates store modelo0
estimates replay
estimates replay modelo0


svyset correlativo [pw=fac00],  ///
strata(estratoarea) vce(linearized) singleunit(certainty)

svy: regress log_money aproba1 if actpr2012==10 
estimates store modelo1

esttab modelo0 modelo1


*** Predicciones

estimates restore modelo0

predict y_hat if _est_modelo0==1

predict errores if _est_modelo0==1, res


*** Análisis de supuestos

** Normalidad

summ errores

histogram errores

sktest errores

** H0: los errores son normales
** HA: los errores no son normales

* Rechazamos la normalidad

** Heterocedasticidad

estat hettest
* HO: Los errores tienen varianza constante
* HA: los errores no tienen varianza constante.

** Rechazamos homocedasticidad.


** Datos atípicos

estimates restore modelo0
estat summarize

predict residuos_est if _est_modelo0, rstandard

predict leverage if _est_modelo0==1, leverage 
* Cuando  leverage > 2k/n es "alta"


estimates restore modelo0
dfbeta


predict cooksd if _est_modelo0==1, cooksd

regress log_money aproba1 if actpr2012==10 & residuos_est<3

* -----------------------------------------------------------------------------
*  Regresión lineal múltiple
* -----------------------------------------------------------------------------

*** Una nueva variable categórica

tab r104, gen(s_)

regress log_money aproba1 s_2 if actpr2012==10

regress log_money aproba1 i.r104 if actpr2012==10

estimates store modelo2

regress log_money aproba1 ib2.r104 if actpr2012==10

esttab modelo0 modelo1 modelo2, stats(F)


ssc install ftest, replace

ftest modelo0 modelo2

* H0: el modelo0 es igual o mejor que el modelo 2
* HA: el modelo2 es mejor que el modelo0

regress log_money aproba1 i.r104 ib5.region if actpr2012==10
estimates store modelo3

ftest modelo2 modelo3

* H0: el modelo2 es igual o mejor que el modelo 3
* HA: el modelo3 es mejor que el modelo2

*** Introduciendo una variable numérica

regress log_money aproba1 i.r104 ib5.region r106 if actpr2012==10
estimates store modelo4

ftest modelo4 modelo3


esttab modelo*, ar2 label se 


** Analizar los supuestos

** Normalidad multivariada
mvtest normality log_money aproba1 r106 if actpr2012==10


** Normalidad de los errores 

estimates restore modelo4
predict errores4 if _est_modelo4==1, res // errores
predict y_hat4 if _est_modelo4==1, xb // para el gráfico de heterocedastiticas

histogram errores4 /// vemos que es muy alta y un poco sesgado
sktest errores4 // Rechazamos la normalidad
*HO: Es que los errores son normales
*HA: es que los errores no son normales
* Valores p chicos rechazan la normalidad

** Heterocedasticidad

hettest
*HO: Es que los errores son homocedásticos
*HA: es que los errores no son homocedásticos
* Valores p chicos rechazan la homocedasticidad

scatter errores4 y_hat4 if _est_modelo4==1, jitter(20) mcolor(%20)

rvfplot // residuals vs fitted plot


* ¿Cómo se corrige la heterocedaticidad?*

* Afecta a los errores estándar
* Subestima los errores estándar

estimate replay modelo4
regress log_money aproba1 i.r104 ib5.region r106 if actpr2012==10,  robust
regress log_money aproba1 i.r104 ib5.region r106 if actpr2012==10,  vce(hc3)
estimates store modelo4_hc3

esttab modelo4*, label se

regress log_money aproba1 i.r104 ib5.region r106 if actpr2012==10,  cluster(idboleta)


*** Multicolinelidad

estimate restore modelo4
vif

regress log_money s_1 s_2 if actpr2012==10


*** Post-estimaciones

* margins

estimates restore modelo4
margins r104


estimates restore modelo4
margins r104#region


estimates restore modelo4
margins r104##region


estimates restore modelo4
margins, dydx(*)

estimates restore modelo4

margins, expression(exp(predict(xb))*exp((`e(rmse)'^2)/2))


margins r104, expression(exp(predict(xb))*exp((`e(rmse)'^2)/2))

margins r104, expression(exp(predict(xb))*exp((`e(rmse)'^2)/2))

mean money if _est_modelo4==1, over(r104)


*** Tablas de resultados
estimates restore modelo4
ereturn list


esttab modelo0 modelo2 modelo3 modelo4, stats(r2 r2_a F) label se
esttab modelo0 modelo2 modelo3 modelo4 using "out/resultados.rtf", stats(r2 r2_a F) label se replace
esttab modelo0 modelo2 modelo3 modelo4 using "out/resultados.csv", stats(r2 r2_a F) label se replace


*** Gráficos de coeficientes
coefplot modelo0 modelo2 modelo3 modelo4, drop(_cons) title("Resultados de los modelos") xline(0)

estimates restore modelo4*

margins , at(r106=(20(5)50)) expression(exp(predict(xb))*exp((`e(rmse)'^2)/2))
marginsplot, title("Ingresos predichos") ytitle("Dólares mensuales")

log close


