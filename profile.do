clear
use "datos/ehpm_2019", clear


gen log_money=log(money) if actpr2012==10

clear
use "datos/ehpm_2019", clear


gen log_money=log(money) if actpr2012==10

regress log_money aproba1 i.r104 ib5.region r106 if actpr2012==10
estimates store modelo4

graph bar (mean) money if actpr2012==10, over(aproba1)

graph bar (mean) log_money if actpr2012==10, over(aproba1)

graph bar (mean) log_money if actpr2012==10, over(aproba1)
graph export "images/cuadratico.png", replace width(1000)
gen aproba2=aproba1^2
twoway scatter log_money aproba2 if actpr2012==10, jitter(50)  mcolor(%20)
graph export "images/scatter_cuadri.png", replace width(1000)

twoway scatter log_money aproba2 if actpr2012==10, jitter(50)  mcolor(%20)
pwcorr log_money aproba* if actpr2012==10, sig obs

regress log_money aproba1 aproba2 i.r104 ib5.region r106 if actpr2012==10
estimates store modelo5
ftest modelo4 modelo5
estimates restore modelo5
margins, dydx(aproba*)
regress log_money c.aproba1##c.aproba1 i.r104 ib5.region r106 if actpr2012==10
estimates store modelo6
estimates restore modelo6
margins, dydx(aproba1)
regress log_money c.aproba1##i.r104 ib5.region r106 if actpr2012==10
estimates store modelo7

estimates restore modelo4
margins r104, at(aproba1=(0(4)25))
marginsplot

estimates restore modelo4
margins r104, at(aproba1=(0(4)25))
marginsplot
graph export "images/marginsplot4.png", replace width(1000)

estimates restore modelo7
margins r104, at(aproba1=(0(4)25))
marginsplot

