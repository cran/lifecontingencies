### R code from vignette source 'lifecontingenciesVignettes.Rnw'

###################################################
### code chunk number 1: lifecontingenciesVignettes.Rnw:80-83
###################################################
	options(prompt = "R> ", continue = "+ F ", width = 70, useFancyQuotes = FALSE)
	set.seed(123)
	numSim=20000


###################################################
### code chunk number 2: load
###################################################
library("lifecontingencies")


###################################################
### code chunk number 3: show classes
###################################################
showClass("lifetable")


###################################################
### code chunk number 4: show actuarial
###################################################
showClass("actuarialtable")


###################################################
### code chunk number 5: ir1
###################################################
real2Nominal(0.03,12)


###################################################
### code chunk number 6: ir2
###################################################
nominal2Real(0.06,12)


###################################################
### code chunk number 7: ir3
###################################################
real2Nominal(i=0.04,k=12,type="discount")


###################################################
### code chunk number 8: npv1
###################################################
capitals=c(-1000,200,500,700)
times=c(0,1,2,5)
presentValue(cashFlows=capitals, timeIds=times,interestRates=0.03)


###################################################
### code chunk number 9: npv2
###################################################
presentValue(cashFlows=capitals, timeIds=times, 
interestRates=c( 0.04, 0.02, 0.03, 0.05))


###################################################
### code chunk number 10: npv3
###################################################
presentValue(cashFlows=capitals, timeIds=times, 
interestRates=c( 0.04, 0.02, 0.03, 0.05), 
probabilities=c(1,1,1,0.5))


###################################################
### code chunk number 11: ann1
###################################################
100*annuity(i=0.03,n=5)


###################################################
### code chunk number 12: ann2
###################################################
100*accumulatedValue(i=0.03,n=5)


###################################################
### code chunk number 13: ann3
###################################################
C=100000
R=C/accumulatedValue(i=0.05,n=10)
R


###################################################
### code chunk number 14: ann4
###################################################
100*4*annuity(i=nominal2Real(0.06,2),n=4,k=4)


###################################################
### code chunk number 15: ann5
###################################################
incrAnn<-increasingAnnuity(i=0.03, n=10,type="due")
decrAnn<-decreasingAnnuity(i=0.03, n=10,type="immediate")
c(incrAnn, decrAnn)


###################################################
### code chunk number 16: ann6
###################################################
annuity(i=((1+0.04)/(1+0.03)-1),n=10)


###################################################
### code chunk number 17: capAmort1
###################################################
capital=100000
interest=0.05 
payments_per_year=2
rate_per_period=(1+interest)^(1/payments_per_year)-1
years=30
R=
1/payments_per_year*capital/annuity(i=interest, 
n=years,k=payments_per_year)
R	


###################################################
### code chunk number 18: capAmort2
###################################################
balanceDue=numeric(years*payments_per_year)
balanceDue[1]=capital*(1+rate_per_period)-R
for(i in 2:length(balanceDue)) balanceDue[i]=balanceDue[i-1]*(1+rate_per_period)-R


###################################################
### code chunk number 19: figBalanceDue
###################################################
plot(x=c(1:length(balanceDue)),y=balanceDue, main="Loan amortization", 
		ylab="EoP balance due", xlab="year", type="l",col="steelblue")


###################################################
### code chunk number 20: BPFun1
###################################################
bond<-function(faceValue, couponRate, couponsPerYear, yield,maturity)
{
	out=NULL
	numberOfCF=maturity*couponsPerYear
	CFs=numeric(numberOfCF)
	payments=couponRate*faceValue/couponsPerYear 
	cf=payments*rep(1,numberOfCF)
	cf[numberOfCF]=faceValue+payments 
	times=seq.int(from=1/couponsPerYear, to=maturity, by=maturity/numberOfCF)
	out=presentValue(cashFlows=cf, interestRates=yield, timeIds=times)
	return(out)
}


###################################################
### code chunk number 21: BPFun2
###################################################
bndEx1<-bond(1000,0.06,2,0.05,3)
bndEx2<-bond(1000,0.06,2,0.06,3)
c(bndEx1, bndEx2)


###################################################
### code chunk number 22: duration and convexity
###################################################
cashFlows=c(100,100,100,600,500,700)
timeVector=seq(1:6)
interestRate=0.03
ex1<-duration(cashFlows=cashFlows, timeIds=timeVector, 
		i=interestRate, k = 1, macaulay = TRUE)
ex2<-duration(cashFlows=cashFlows, timeIds=timeVector, 
		i=interestRate, k = 1, macaulay = FALSE)
ex3<-convexity(cashFlows=cashFlows, timeIds=timeVector, 
		i=interestRate, k = 1)
c(ex1, ex2,ex3)


###################################################
### code chunk number 23: create a lifecontingencies object
###################################################
x_example=seq(from=0,to=9, by=1)
lx_example=c(1000,950,850,700,680,600,550,400,200,50)
exampleLt=new("lifetable",x=x_example, lx=lx_example, name="example lifetable")


###################################################
### code chunk number 24: print - show
###################################################
print(exampleLt)


###################################################
### code chunk number 25: head and tail
###################################################
head(exampleLt)


###################################################
### code chunk number 26: fromDataFrame1
###################################################
data("demoUsa")
data("demoIta") 
usaMale07=demoUsa[,c("age", "USSS2007M")]
usaMale00=demoUsa[,c("age", "USSS2000M")]
names(usaMale07)=c("x","lx")
names(usaMale00)=c("x","lx")
usaMale07Lt<-as(usaMale07,"lifetable")
usaMale07Lt@name="USA MALES 2007"
usaMale00Lt<-as(usaMale00,"lifetable")
usaMale00Lt@name="USA MALES 2000"


###################################################
### code chunk number 27: fromDataFrame2
###################################################
lxIPS55M<-with(demoIta, IPS55M)
pos2Remove<-which(lxIPS55M %in% c(0,NA))
lxIPS55M<-lxIPS55M[-pos2Remove]
xIPS55M<-seq(0,length(lxIPS55M)-1,1)

lxIPS55F<-with(demoIta, IPS55F)
pos2Remove<-which(lxIPS55F %in% c(0,NA))
lxIPS55F<-lxIPS55F[-pos2Remove]
xIPS55F<-seq(0,length(lxIPS55F)-1,1)

ips55M=new("lifetable",x=xIPS55M, lx=lxIPS55M, 
		name="IPS 55 Males")
ips55F=new("lifetable",x=xIPS55F, lx=lxIPS55F, 
		name="IPS 55 Females")


###################################################
### code chunk number 28: create from survival rates
###################################################

data("demoIta")
itaM2002<-demoIta[,c("X","SIM92")]
names(itaM2002)=c("x","lx")
itaM2002Lt<-as(itaM2002,"lifetable")
itaM2002Lt@name="IT 2002 Males"

itaM2002<-as(itaM2002Lt,"data.frame")
itaM2002$qx<-1-itaM2002$px

for(i in 20:60) itaM2002$qx[itaM2002$x==i]=0.2*itaM2002$qx[itaM2002$x==i]

itaM2002reduced<-probs2lifetable(probs=itaM2002[,"qx"], radix=100000,
		type="qx",name="IT 2002 Males reduced")


###################################################
### code chunk number 29: create a actuarialtable object
###################################################

exampleAct=new("actuarialtable",x=exampleLt@x, lx=exampleLt@lx, 
interest=0.03, name="example actuarialtable")


###################################################
### code chunk number 30: methods1
###################################################
getOmega(exampleAct)


###################################################
### code chunk number 31: methods2
###################################################
print(exampleLt)
print(exampleAct)


###################################################
### code chunk number 32: figSurvivalFunction
###################################################
	plot(soa08Act, type="l",col="steelblue")


###################################################
### code chunk number 33: probability and demographics
###################################################
demoEx1<-pxt(ips55M,20,1)
demoEx2<-qxt(ips55M,30,2) 
demoEx3<-exn(ips55M, 50,20) 
c(demoEx1,demoEx2,demoEx3)


###################################################
### code chunk number 34: fractional ages
###################################################
data("soa08Act")
pxtLin=pxt(soa08Act,80,0.5,"linear") 
pxtCnst=pxt(soa08Act,80,0.5,"constant force") 
pxtHyph=pxt(soa08Act,80,0.5,"hyperbolic") 
c(pxtLin,pxtCnst,pxtHyph)


###################################################
### code chunk number 35: more than one head
###################################################
jsp=pxyt(ips55M,ips55F,x=65, y=63, t=2)
lsp=pxyt(ips55M,ips55F,x=65, y=63, t=2,status="last") 
jelt=exyt(ips55M, ips55F, x=65,y=63, status="joint") 
c(jsp,lsp,jelt)


###################################################
### code chunk number 36: life insurance 1
###################################################
lins1=Axn(soa08Act, 30,10,i=0.04)
lins2=Axn(soa08Act, x=30,n=10,i=0.04,k=12)
lins3=Axn(soa08Act, 40) 
lins4=Axn(soa08Act, x=40,n=10,m=5,i=0.05) 
lins5=DAxn(soa08Act, 50,5)
lins6=IAxn(soa08Act, 40,10)
c(lins1,lins2,lins3,lins4,lins5,lins6)


###################################################
### code chunk number 37: pure endowments
###################################################
puEnd1<-Exn(soa08Act, x=30, n=35, i=0.06)
puEnd2<-Exn(soa08Act, x=30, n=35, i=0.03)
c(puEnd1,puEnd2)


###################################################
### code chunk number 38: annuities
###################################################
annEx1<-axn(soa08Act, x=65, m=1)
annEx2<-axn(soa08Act, x=65)
annEx3<-12*1000*axn(soa08Act, x=65,k=12)
annEx4<-12*1000*axn(soa08Act, x=65,k=12, n=20)
annEx5<-12*1000*axn(soa08Act, x=65,k=12,n=20,m=1/12)
c(annEx1,annEx2,annEx3,annEx4,annEx5)


###################################################
### code chunk number 39: life insurance 2
###################################################
APV=100000*Axn(soa08Act, x=30,n=35,i=0.025)
Pa=APV/axn(soa08Act, x=30,n=15,i=0.025)
Pm=APV/(12*axn(soa08Act, x=30,n=15,i=0.025,k=12))
c(Pa,Pm)


###################################################
### code chunk number 40: life insurance benefit reserve
###################################################
P=100000*Axn(soa08Act,x=25,n=40,i=0.03)/axn(soa08Act,x=25,n=40,i=0.03)
reserveFun=function(t) return(100000*Axn(soa08Act,x=25+t,n=40-t,i=0.03)-P*
					axn(soa08Act,x=25+t,n=40-t,i=0.03))
for(t in 0:40) {if(t%%5==0) cat("At time ",t,
				" benefit reserve is ", reserveFun(t),"\n")}


###################################################
### code chunk number 41: annuity reserve
###################################################
yearlyRate=12000
irate=0.02
APV=yearlyRate*axn(soa08Act, x=25, i=irate,m=65-25,k=12)
levelPremium=APV/axn(soa08Act, x=25,n=65-25,k=12)

annuityReserve<-function(t) {
	out<-NULL
	if(t<65-25) out=yearlyRate*axn(soa08Act, x=25+t, i=irate,m=65-(25+t),k=12)-levelPremium*axn(soa08Act, x=25+t,n=65-(25+t),k=12) else {
		out=yearlyRate*axn(soa08Act, x=25+t, i=irate,k=12)
	}
	return(out)
}

years=seq(from=0, to=getOmega(soa08Act)-25-1,by=1)
annuityRes=numeric(length(years))
for(i in years) annuityRes[i+1]=annuityReserve(i)
dataAnnuityRes<-data.frame(years=years, reserve=annuityRes)


###################################################
### code chunk number 42: annuityReserveGraph
###################################################

plot(y=dataAnnuityRes$reserve, x=dataAnnuityRes$years,
col="steelblue", main="Deferred Annuity Benefit Reserve",
ylab="amount",xlab="years",type="l")



###################################################
### code chunk number 43: exp augmented
###################################################
G=(100000*Axn(soa08Act, x=35)+ 275)/(1-.1)
G


###################################################
### code chunk number 44: two heads annuity immediate
###################################################
ex1<-axn(soa08Act, x=65,m=1)+axn(soa08Act, x=70,m=1)-
		axyn(soa08Act,soa08Act,	x=65,y=70,status="joint",m=1) 
ex2<-axyn(soa08Act,soa08Act, x=65,y=70, status="last",m=1)
ex1-ex2


###################################################
### code chunk number 45: revesionary annuity
###################################################
axn(soa08Act, x=60,m=1)-axyn(soa08Act,soa08Act, x=65,y=60,status="joint",m=1)


###################################################
### code chunk number 46: rLife1
###################################################
data(soa08Act)
sample1<-rLife(n=10,object=soa08Act,x=0,type="Tx")
sample2<-rLife(n=10,object=soa08Act,x=0,type="Kx")


###################################################
### code chunk number 47: rLife2
###################################################
exn(soa08Act, x=29,type="curtate")
t.test(x=rLife(2000,soa08Act, x=29,type="Kx"),
		mu=exn(soa08Act, x=29,type="curtate"))$p.value
deathsIPS55M<-rLife(n=numSim,ips55M, x=0, type="Kx")



###################################################
### code chunk number 48: deathsIPS55Mfig
###################################################
hist(deathsIPS55M, freq=FALSE, main="IPS55M Table Kx Distribution",
			xlab="Age until death",col="steelblue",nclass=100)


###################################################
### code chunk number 49: Axn APV and stochastic
###################################################

APVAxn=Axn(soa08Act,x=25,n=40,type="EV")
APVAxn
sampleAxn=rLifeContingencies(n=numSim, lifecontingency="Axn",
		object=soa08Act,x=25,t=40,parallel=TRUE)
tt1<-t.test(x=sampleAxn,mu=APVAxn)$p.value

APVIAxn=IAxn(soa08Act,x=25,n=40,type="EV")
APVIAxn
sampleIAxn=rLifeContingencies(n=numSim, lifecontingency="IAxn",
		object=soa08Act,x=25,t=40,parallel=TRUE)
tt2<-t.test(x=sampleIAxn,mu=APVIAxn)$p.value

APVaxn=axn(soa08Act,x=25,n=40,type="EV")
APVaxn
sampleaxn=rLifeContingencies(n=numSim, lifecontingency="axn",
		object=soa08Act,x=25,t=40,parallel=TRUE)
tt3<-t.test(x=sampleaxn,mu=APVaxn)$p.value

APVAExn=AExn(soa08Act,x=25,n=40,type="EV")
APVAExn
sampleAExn=rLifeContingencies(n=numSim, lifecontingency="AExn",
		object=soa08Act,x=25,t=40,parallel=TRUE)
tt4<-t.test(x=sampleAExn,mu=APVAExn)$p.value
c(tt1, tt2,tt3, tt4)


###################################################
### code chunk number 50: figsim
###################################################
	par(mfrow=c(2,2))
	hist(sampleAxn, main="Term Insurance",xlab="Actuarial present value",nclass=50, col="steelblue",freq=FALSE);abline(v=APVAxn, col="red", lwd=2)
	hist(sampleIAxn, main="Increasing Life Insurance",xlab="Actuarial present value",nclass=50, col="steelblue",freq=FALSE);abline(v=APVIAxn, col="red", lwd=2)
	hist(sampleaxn, main="Temporary Annuity Due",xlab="Actuarial present value",nclass=50, col="steelblue",freq=FALSE);abline(v=APVaxn, col="red", lwd=2)
	hist(sampleAExn, main="Endowment Insurance",xlab="Actuarial present value",nclass=50, col="steelblue",freq=FALSE);abline(v=APVAExn, col="red", lwd=2)


###################################################
### code chunk number 51: stochastic example full 1
###################################################
nsim=100
employees=500
salaryDistribution=rlnorm(n=employees,m=10.77668944,s=0.086177696)
ageDistribution=round(runif(n=employees,min=25, max=65))
policyLength=sapply(65-ageDistribution, min,1)

getEmployeeBenefit<-function(index,type="EV") {
	out=numeric(1)
	out=salaryDistribution[index]*Axn(actuarialtable=soa08Act, 
			x=ageDistribution[index],n=policyLength[index], 
			i=0.02,m=0,k=1, type=type)
	return(out)
}

require(parallel)
cl <- makeCluster(detectCores())
worker.init <- function(packages) {
	for (p in packages) {
		library(p, character.only=TRUE)
	}
	invisible(NULL)
}
clusterCall(cl, 
		worker.init, c('lifecontingencies'))
clusterExport(cl, varlist=c("employees","getEmployeeBenefit",
				"salaryDistribution","policyLength",
				"ageDistribution","soa08Act"))


###################################################
### code chunk number 52: stochastic example full 2
###################################################
employeeBenefits=numeric(employees)
employeeBenefits<- parSapply(cl, 1:employees,getEmployeeBenefit, type="EV")
employeeBenefit=sum(employeeBenefits)



benefitDistribution=numeric(nsim)
yearlyBenefitSimulate<-function(i)
{
	out=numeric(1)
	expenseSimulation=numeric(employees)
	expenseSimulation=sapply(1:employees, getEmployeeBenefit, type="ST")
	out=sum(expenseSimulation)
	return(out)
}

benefitDistribution <- parSapply(cl, 1:nsim,yearlyBenefitSimulate )
stopCluster(cl)

riskMargin=as.numeric(quantile(benefitDistribution,.75)-employeeBenefit)
totalBookedCost=employeeBenefit+riskMargin

employeeBenefit
riskMargin
totalBookedCost


