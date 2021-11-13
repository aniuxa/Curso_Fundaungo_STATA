* -----------------------------------------------------------------------------
* -----------------------------------------------------------------------------
* SESIÓN 6
* CURSO STATA FUNDAUNGO
* AUTORA: ANA ESCOTO
* FECHA: 13/11/2021 - última modificación: actualicé según la clase
* --------------------------------------------------------------------------------
* -----------------------------------------------------------------------------

capture log close // cierra cualquier log abierto
set more off // Quita el boton de "more" en la ventana de resultados
clear // cierra cualquier base de datos abierta

* findit renvars
* ssc install sq, replace
* ssc install moremata
*search SADI


* -----------------------------------------------------------------------------
*  ESTABLECIENDO DIRECTORIOS 
* -----------------------------------------------------------------------------

gl computer "C:\Users\anaes\Dropbox\2021\Curso_Fundaungo_STATA\"
gl datos "$computer\datos"
gl temp "$computer\datos\temp"

cd "$computer\" 

log using "sesion6.log", replace


* -----------------------------------------------------------------------------
*  Importando las bases
* -----------------------------------------------------------------------------

use "$datos/2021_11_01_Fundaungo", clear 
renvars , lower


* -----------------------------------------------------------------------------
*  Índices con escalas de likert
* -----------------------------------------------------------------------------

gl xconfianza  ci1-ci8

* Yo preferiría que hubiera un 0

foreach var of varlist $xconfianza {
	recode `var'(1=2) (2=1) (3=-1) (4=-2) (88=.) (99=.), gen(i_`var')
}

gl xconfianza2  i_ci*

egen index=rowtotal( $xconfianza2 ), miss
replace index=index/8

histogram index
** Tiene un comportamiento más contínuo entre más opciones. Y sin el cero es posible que esté menos centrada

* -----------------------------------------------------------------------------
* Preparando la base para análisis de secuencia
* Necesitamos long. 
* -----------------------------------------------------------------------------
 
gen id_miss=(id==.)

drop if id_miss==1 // nos vamos a quedar con el mismo inicio
isid correlativo
 
renvars se1-l1  pol2 nb2 id1 , postfix(_1)

gl inicio correlativo id ronda departamento se1_1-ac2_1

forvalues i=1(1)5 {
 global onda_`i' se2_`i' sit1_`i'  id1_`i'
}

keep $inicio $onda_1 $onda_2 $onda_3 $onda_4 $onda_5

* -----------------------------------------------------------------------------
*  Reshape
* -----------------------------------------------------------------------------

/* Para hacer este ejercicio más claro, vamos a botar algunas variables*/

reshape long se2_ sit1_  id1_, i(id) j(wave)
drop if ronda>1

recode id1 (99=.)
* -----------------------------------------------------------------------------
*  Declaración de la base
* -----------------------------------------------------------------------------
sqset id1 id wave, keeplongest


*ANÁLISIS DESCRIPTIVO DE LAS SECUENCIAS
*frecuencia de las secuencias
sqtab
sqtab if se1_1==0
sqtab if se1_1==1

*las 20 secuencias más frecuentes
sqtab, ranks(1/20)
sqtab if se1_1==0, ranks(1/20)
sqtab if se1_1==1, ranks(1/20)

*SO secuencias con el mismo orden de elementos
sqtab, ranks(1/10) so

*SE secuencias con los mismos elementos
sqtab, ranks(1/10) se

*concentración
sqdes 

*descripción de las secuencias
egen length=sqlength()
egen length1=sqlength(), element(1)
egen length2=sqlength(), element(2)
egen length3=sqlength(), element(3)
egen length4=sqlength(), element(4)
egen elemnum=sqelemcount()
egen epinum=sqepicount()

sqstatsum

sqstattab1 elemnum //las secuencias están compuestas principalmente de 2 o 3 elementos

sqstattabsum se1_1

*gráficamente...
sqindexplot, ///
	ytitle(Secuencias) xtitle(Ronda) xlabel(1(1)4) ///
	graphregion(fcolor(white)) 
	


sqparcoord, ylabel(0(1)9, valuelabel angle(0)) wlines(2)



*COMPARACIÓN DE LAS SECUENCIAS: OPTIMAL MATCHING
*con costos default: indel=1,sub=2
sqom, full
sqclusterdat
clustermat wardslinkage SQdist, name(wards) add  	 	
cluster tree wards, cutnumber(10)
cluster generate cluster5 = groups(5)
sqclusterdat, return

by id: gen n_ent=_n
tab cluster5 if n_ent==1

*Tipo ideal de trayectoria por cluster
sqmodalplot, by(cluster5) graphregion(fcolor(white)) 

*por sexo
sqmodalplot, over(cluster5) by(se1_1) graphregion(fcolor(white)) 


*Qué hay en los cluster?
*gráficamente
sqindexplot, by(cluster5)

*Secuencias en cada cluster
sqtab if cluster5==1
sqtab if cluster5==2
sqtab if cluster5==3
sqtab if cluster5==4
sqtab if cluster5==5

mlogit cluster5 i.se1_1 se2_ if n_ent==1
estimates store mlogit
estadd fitstat

foreach o in 1 2 3 4 5 {
    margins, dydx(*) predict(outcome(`o')) post
    estimates store out`o', title(Outcome `o')
    estimates restore mlogit
}

coefplot  out1 out2 out3 out4 out5 ,  xline(0)  xline(0) msymbol(d) ///
 mfcolor(white) 
 

log close