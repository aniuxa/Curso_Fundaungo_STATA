* Este es mi primer do-file 
cd "C:\Users\anaes\Dropbox\PC\Downloads\Curso_Fundaungo_STATA-main\Curso_Fundaungo_STATA-main" // Establezco directorio de trabajo

use "datos\ehpm_2019.dta", clear

describe // describe todas las variables

describe r421a-znorte // describe desde r421a hasta znortr
describe gas* // describe las variables que empiezan con gas

su // da n, media, desviación estándar, min y max de todas las variables de la base
su money // da n, media, desviación estándar, min y max de la variable money
su money, detail // da más estadísticas, como percentiles, simetría y curtosis

* IMPORTACIÓN DE OTROS FORMATOS

clear

* Para importar archivos separados por comas

import delimited "datos\ejemplo_csv.csv", clear

* Para importar archivos de excel

import excel "datos\ejemplo_xlsx.xlsx", sheet("para_importar") firstrow clear 


** Para importar archivos de dBase

import dbase "datos\ejemplo_dbf.dbf", clear

*** Volvemos a la ehpm_2019

use "datos\ehpm_2019.dta", clear

gen x=1
replace x=0 if r104==1

tab x

drop x

preserve // empiezan comandos reversibles
drop if r104==1 // bota casos que cumplan con la condición
tab r104 // vemos 	que se han botado los casos
restore // vuelve a la base original

preserve // empiezan comandos reversibles
keep if r104==1 // se queda con casos que cumplan con la condición
tab r104 // vemos 	que se han botado los casos
restore // vuelve a la base original

preserve // empiezan comandos reversibles
keep r* // se queda con las variables que empiezan en r
describe  // vemos 	que se han botado los casos
restore // vuelve a la base original


*** Uso de clonevar

*diferencias con generate:

gen x1=r104

clonevar x2=r104

describe x1 x2 r104

tab x1
tab x2


***** EGEN


* Primer ejemplo con la función cut

egen edad5=cut(r106), at(0(5)100)


** Segundo ejemplo con la función mode

egen moda_edad=mode(r106)

drop moda_edad

* Cambiando algunas opciones

egen moda_edad=mode(r106), maxmode


** Tercer ejemplo

egen mean_edad=mean(r106), by(region)


*** Recodificación de variables. 

tab edad5

recode edad5 (65/100=65)
recode edad5 (65/100=65) (0/10=.), gen(pet5)

tab pet5


* ETIQUETADO DE VARIABLES

label variable pet5 "Edad quinquenal PET"

labelbook

labelbook r104

label define sexlabel 1 "Male" 2 "Female"
label values r104 x2 sexlabel
label values r104 r104

label define sexlabel 3 "Non-binary", modify


*** Tabulados

tab r104
tab r104, summ(r106) // da un  resumen de la variable 106 para c/categoría

tab r104, nolabel // tabulado sin etiquetas

tab pet5, miss // muestra los valores missing

tab r104, gen(s_)


** Factor de expansión

tab r104 [iw=fac00]

** Tabulados de doble 

tab actpr r104 [iw=fac00]

tab actpr r104 [iw=fac00], row

tab actpr r104 [iw=fac00], col

tab actpr r104 [iw=fac00], cell


* Table

table region actpr2012 r104 [iw=fac00]

table region actpr2012 r104 [iw=fac00], stat(percent)
table (region actpr2012) r104 [iw=fac00], stat(percent)
table region (actpr2012 r104) [iw=fac00], stat(percent)

table region actpr2012 r104 [iw=fac00], stat(percent,total) // default
table region actpr2012 r104 [iw=fac00], stat(percent, across (r104))
table region actpr2012 r104 [iw=fac00], stat(percent, across (actpr2012))


* Tabla de estadísticas

tabstat money if actpr2012==10 [aw=fac00]

tabstat money if actpr2012==10 [aw=fac00], s(mean p50 sd q )

tabstat money if actpr2012==10 [aw=fac00], s(mean p50 sd q ) by(r104)

** Gráficos

histogram money if actpr2012==10, normal

kdensity money if actpr2012==10


graph box money if actpr2012==10

graph box r106

graph hbox money if actpr2012==10, over(r104)
