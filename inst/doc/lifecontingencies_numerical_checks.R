### R code from vignette source 'lifecontingencies_numerical_checks.Rnw'

###################################################
### code chunk number 1: lifecontingencies_numerical_checks.Rnw:26-27
###################################################
options(width=80, prompt='R> ')


###################################################
### code chunk number 2: load
###################################################
#load lifecontingencies package
library(lifecontingencies)


###################################################
### code chunk number 3: act1
###################################################
data(soa08Act)
#should be 0.01577285
Axn(soa08Act, x=30, n=10, i=0.04)


###################################################
### code chunk number 4: act2
###################################################
#should be 0.01247099
Axn(soa08Act, x=30, n=10, i=0.04,power=2)-Axn(soa08Act, x=30, n=10, i=0.04,power=1)^2


###################################################
### code chunk number 5: act3
###################################################
#should be 102.4835
1000*Axn(soa08Act, x=30,i=0.06)


###################################################
### code chunk number 6: act4
###################################################
#should be 0.01155
Axn(soa08Act, 50,20)/axn(soa08Act, 50,20)


