### R code from vignette source 'lifecontingenciesVignettes.Rnw'

###################################################
### code chunk number 1: lifecontingenciesVignettes.Rnw:79-83
###################################################
	options(prompt = "R> ", continue = "+ F ", width = 70, useFancyQuotes = FALSE)
	library(lifecontingencies)
	source("calculations1.R")
	source("calculations2.R")


###################################################
### code chunk number 2: load
###################################################
library(lifecontingencies)


###################################################
### code chunk number 3: show classes
###################################################
#definition of lifetable
showClass("lifetable")


###################################################
### code chunk number 4: show actuarial
###################################################
showClass("actuarialtable")


###################################################
### code chunk number 5: interest rates functions
###################################################
#an APR of 3% is equal to a 
real2Nominal(0.03,12)
#nominal interest rate 
#while 6% annual nominal interest rate is the same of 
nominal2Real(0.06,12)

#4% effective interest rate corresponds to 
real2Nominal(0.04,4)*100
#nominal interest rate (in 100s) compounded quarterly

#an effective rate of discount of 4% is equal to a 
real2Nominal(i=0.04,k=12,type="discount")
#nominal rate of discount payable quarterly


###################################################
### code chunk number 6: present value
###################################################
#varing cash flow pattern
capitals=c(-1000,200,500,700)
times=c(0,1,2,5)
ex1<-presentValue(cashFlows=capitals, timeIds=times, 
             interestRates=0.03)
#varying interest rates
ex2<-presentValue(cashFlows=capitals, timeIds=times, 
             interestRates=c( 0.04, 0.02, 0.03, 0.05))
#uncertain cash flows
ex3<-presentValue(cashFlows=capitals, timeIds=times, 
interestRates=c( 0.04, 0.02, 0.03, 0.05), probabilities=c(1,1,1,0.5))
c(ex1,ex2,ex3)


###################################################
### code chunk number 7: annuities
###################################################
#the PV of an annuity immediate $100 payable at the end of next 5 years at 3% is
100*annuity(i=0.03,n=5)
#while the corresponding future value is
100*accumulatedValue(i=0.03,n=5)


###################################################
### code chunk number 8: savings account projection
###################################################
C=100000
R=C/accumulatedValue(i=0.05,n=10)
R


###################################################
### code chunk number 9: fractional annuities
###################################################
100*4*annuity(i=nominal2Real(0.06,2),n=4,k=4)


###################################################
### code chunk number 10: increasing and decreasing
###################################################
#increasing annuity example
ex1<-increasingAnnuity(i=0.03, n=10,type="due")
#decreasing annuity example
ex2<-decreasingAnnuity(i=0.03, n=10,type="immediate")
c(ex1,ex2)


###################################################
### code chunk number 11: geometrically increasing annuity
###################################################
annuity(i=((1+0.04)/(1+0.03)-1),n=10)


###################################################
### code chunk number 12: capital amortization
###################################################
capital=100000
interest=0.05 
payments_per_year=2
rate_per_period=(1+interest)^(1/payments_per_year)-1
years=30
installment=
1/payments_per_year*capital/annuity(i=interest, n=years,k=payments_per_year)
installment	
#compute the balance due at the beginning of each period
balance_due=numeric(years*payments_per_year)
balance_due[1]=capital*(1+rate_per_period)-installment
for(i in 2:length(balance_due)) balance_due[i]=balance_due[i-1]*(1+rate_per_period)-installment


###################################################
### code chunk number 13: figBalanceDue
###################################################
plot(x=c(1:length(balance_due)),y=balance_due, main="Loan amortization", 
		ylab="EoP balance due", xlab="year", type="l",col="steelblue")


###################################################
### code chunk number 14: bond pricing
###################################################
#define a function to compute bond market value
bond<-function(faceValue, couponRate, couponsPerYear, yield,maturity)
{
	out=NULL
	numberOfCF=maturity*couponsPerYear #determine the number of CF
	CFs=numeric(numberOfCF)
	payments=couponRate*faceValue/couponsPerYear #determine the coupon sum
	cf=payments*rep(1,numberOfCF)
	cf[numberOfCF]=faceValue+payments #set the last payment amount
	times=seq.int(from=1/couponsPerYear, to=maturity, by=maturity/numberOfCF)
	out=presentValue(cashFlows=cf, interestRates=yield, timeIds=times)
	return(out)
}
#coupon rate 6%, two coupons per year, face value 1000, 
#yield 5%, three years to maturity
bndEx1<-bond(1000,0.06,2,0.05,3)

#coupon rate 3%, one coupons per year, 
#face value 1000, yield 3%, three years to maturity
bndEx2<-bond(1000,0.06,1,0.06,3)
c(bndEx1, bndEx2)


###################################################
### code chunk number 15: duration and convexity
###################################################
#set cash flows, times and interest rates
cashFlows=c(100,100,100,600,500,700)
timeVector=seq(1:6)
interestRate=0.03

#dollar duration
duration(cashFlows=cashFlows, timeIds=timeVector, 
		i=interestRate, k = 1, macaulay = FALSE)

#Macaulay duration
duration(cashFlows=cashFlows, timeIds=timeVector, 
		i=interestRate, k = 1, macaulay = TRUE)

#convexity
convexity(cashFlows=cashFlows, timeIds=timeVector, 
		i=interestRate, k = 1)


###################################################
### code chunk number 16: create a lifecontingencies object
###################################################
x_example=seq(from=0,to=9, by=1)
lx_example=c(1000,950,850,700,680,600,550,400,200,50)
exampleLt=new("lifetable",x=x_example, lx=lx_example, name="example lifetable")


###################################################
### code chunk number 17: print - show
###################################################
print(exampleLt)


###################################################
### code chunk number 18: head and tail
###################################################
#head method
head(exampleLt)


###################################################
### code chunk number 19: create from data frame
###################################################
#load USA Social Security LT
data(demoUsa) 
usaMale07=demoUsa[,c("age", "USSS2007M")]
usaMale00=demoUsa[,c("age", "USSS2000M")]
#coerce from data.frame to lifecontingencies 
#requires x and lx names
names(usaMale07)=c("x","lx")
names(usaMale00)=c("x","lx")
#apply coerce methods and changes names
usaMale07Lt<-as(usaMale07,"lifetable")
usaMale07Lt@name="USA MALES 2007"
usaMale00Lt<-as(usaMale00,"lifetable")
usaMale00Lt@name="USA MALES 2000"
#compare expected lifetimes
c(exn(usaMale00Lt,0),exn(usaMale07Lt,0))

#load Italian IPS55 tables
##males
lxIPS55M<-with(demoIta, IPS55M)
pos2Remove<-which(lxIPS55M %in% c(0,NA))
lxIPS55M<-lxIPS55M[-pos2Remove]
xIPS55M<-seq(0,length(lxIPS55M)-1,1)
##females
lxIPS55F<-with(demoIta, IPS55F)
pos2Remove<-which(lxIPS55F %in% c(0,NA))
lxIPS55F<-lxIPS55F[-pos2Remove]
xIPS55F<-seq(0,length(lxIPS55F)-1,1)
#finalize the tables
ips55M=new("lifetable",x=xIPS55M, lx=lxIPS55M, 
		name="IPS 55 Males")
ips55F=new("lifetable",x=xIPS55F, lx=lxIPS55F, 
		name="IPS 55 Females")
#compare expected lifetimes
c(exn(ips55M,0),exn(ips55F,0))


###################################################
### code chunk number 20: create from survival rates
###################################################
#use 2002 Italian males life tables
data(demoIta)
itaM2002<-demoIta[,c("X","SIM92")]
names(itaM2002)=c("x","lx")
itaM2002Lt<-as(itaM2002,"lifetable")
itaM2002Lt@name="IT 2002 Males"
#reconvert in data frame
itaM2002<-as(itaM2002Lt,"data.frame")
#add qx
itaM2002$qx<-1-itaM2002$px
#reduce to 20% one year death probability for ages between 20 and 60
for(i in 20:60) itaM2002$qx[itaM2002$x==i]=0.2*itaM2002$qx[itaM2002$x==i]
#otbain the reduced mortality table
itaM2002reduced<-probs2lifetable(probs=itaM2002[,"qx"], radix=100000,
		type="qx",name="IT 2002 Males reduced")


###################################################
### code chunk number 21: create a actuarialtable object
###################################################
#assume 3% interest rate
exampleAct=new("actuarialtable",x=exampleLt@x, lx=exampleLt@lx, interest=0.03, 
		name="example actuarialtable")


###################################################
### code chunk number 22: method omega
###################################################
getOmega(exampleAct)


###################################################
### code chunk number 23: print method
###################################################
#apply method print applied on a life table
print(exampleLt)
#apply method print applied on an actuarial table
print(exampleAct)


###################################################
### code chunk number 24: figSurvivalFunction
###################################################
	plot(soa08Act, type="l",col="steelblue")


###################################################
### code chunk number 25: probability and demographics
###################################################
#using ips55M life table
#probability to survive one year, being at age 20
pxt(ips55M,20,1)
#probability to die within two years, being at age 30
qxt(ips55M,30,2) 
#expected (curtate) life time between 50 and 70 years
exn(ips55M, 50,20) 


###################################################
### code chunk number 26: fractional ages
###################################################
data(soa08Act)
pxtLin=pxt(soa08Act,80,0.5,"linear") #linear interpolation 
pxtCnst=pxt(soa08Act,80,0.5,"constant force") #constant force of mortality
pxtHyph=pxt(soa08Act,80,0.5,"hyperbolic") #hyperbolic assumption
c(pxtLin,pxtCnst,pxtHyph)


###################################################
### code chunk number 27: more than one head
###################################################
jsp=pxyt(ips55M,ips55F,x=65, y=63, t=2) #joint survival probability
lsp=pxyt(ips55M,ips55F,x=65, y=63, t=2,status="last") #last survival probability
jelt=exyt(ips55M, ips55F, x=65,y=63, status="joint") #joint expected lifetime
c(jsp,lsp,jelt)


###################################################
### code chunk number 28: life insurance 1
###################################################
#10 years term life insurance for a 40 years old insurer @ 4% interest  
lins1=Axn(soa08Act, 30,10,i=0.04)
#same as above but payable at the end of month of death
lins2=Axn(soa08Act, x=30,n=10,i=0.04,k=12)
#whole life variation @6% interest rate (implicit in SOA actuarial table) 
lins3=Axn(soa08Act, 40) 
#5-year deferred life insurance, 10 years length, 40 years old, @5% interest rate
lins4=Axn(soa08Act, x=40,n=10,m=5,i=0.05) 
#five years annually decreasing term life insurance, insured aged 50.
lins5=DAxn(soa08Act, 50,5)
#20 years term annually increasing life insurance, age 40
lins6=IAxn(soa08Act, 40,10)
c(lins1,lins2,lins3,lins4,lins5,lins6)


###################################################
### code chunk number 29: pure endowments
###################################################
#evaluate the APV for a n year pure endowment, age x=30, n=35, i=6%
ex1<-Exn(soa08Act, x=30, n=35, i=0.06)
#the same but @ i=3%
ex2<-Exn(soa08Act, x=30, n=35, i=0.03)
c(ex1,ex2)


###################################################
### code chunk number 30: annuities
###################################################
#annuity immediate
ex1<-axn(soa08Act, x=65, m=1)
#annuity due
ex2<-axn(soa08Act, x=65)
#due with monthly payments of $1000 provision
ex3<-12*1000*axn(soa08Act, x=65,k=12)
#due with montly payments of $1000 provision, 20 - years term
ex4<-12*1000*axn(soa08Act, x=65,k=12, n=20)
#immediate with monthly payments of 1000 provision, 20 - years term
ex5<-12*1000*axn(soa08Act, x=65,k=12,n=20,m=1/12)
c(ex1,ex2,ex3,ex4,ex5)


###################################################
### code chunk number 31: life insurance 2
###################################################

#Assume X, aged 30, whishes to buy a $ 250K 35-years life insurance
#premium paid annually for 15 years @2.5% interest rate.
Pa=100000*Axn(soa08Act, x=30,n=35,i=0.025)/axn(soa08Act, x=30,n=15,i=0.025)
#while if the premium is paid on a montly basis the flat benefit premium
Pm=100000*Axn(soa08Act, x=30,n=35,i=0.025)/axn(soa08Act, x=30,n=15,i=0.025,k=12)
c(Pa,Pm)


###################################################
### code chunk number 32: endowment insurance
###################################################
#level semiannual premium for an endowment insurance of 10000
#insured age 50, insurance term is 20 years
APV=10000*(Axn(soa08Act,50,20)+Exn(soa08Act,50,20))
P=APV/axn(soa08Act,50,20,k=2)
P


###################################################
### code chunk number 33: life insurance benefit reserve
###################################################
P=100000*Axn(soa08Act,x=25,n=40,i=0.03)/axn(soa08Act,x=25,n=40,i=0.03)
reserveFun=function(t) return(100000*Axn(soa08Act,x=25+t,n=40-t,i=0.03)-P*
					axn(soa08Act,x=25+t,n=40-t,i=0.03))

for(t in 0:40) {if(t%%5==0) cat("At time ",t,
				" benefit reserve is ", reserveFun(t),"\n")}
			



###################################################
### code chunk number 34: two heads annuity immediate
###################################################
axn(soa08Act, x=65,m=1)+axn(soa08Act, x=70,m=1)-
		axyn(soa08Act,soa08Act,	x=65,y=70,status="joint",m=1) 
axyn(soa08Act,soa08Act, x=65,y=70, status="last",m=1)


###################################################
### code chunk number 35: revesionary annuity
###################################################
#assume x aged 65, y aged 60
axn(soa08Act, x=60,m=1)-axyn(soa08Act,soa08Act, x=65,y=60,status="joint",m=1)


###################################################
### code chunk number 36: rLife1
###################################################
data(soa08Act)
#sample 10 numbers from the Tx distribution
sample1<-rLife(n=10,object=soa08Act,x=0,type="Tx")
#sample 10 numbers from the Kx distribution
sample1<-rLife(n=10,object=soa08Act,x=0,type="Kx")


###################################################
### code chunk number 37: rLife2
###################################################
#assume an insured aged 29
#his expected integer number of years until death is 
exn(soa08Act, x=29,type="curtate")
#check if we are sampling from a statistically equivalent distribution
t.test(x=rLife(2000,soa08Act, x=29,type="Kx"),
		mu=exn(soa08Act, x=29,type="curtate"))$p.value
#statistically not significant


###################################################
### code chunk number 38: deathsIPS55M
###################################################
	hist(deathsIPS55M, freq=FALSE, main="IPS55M table Kx distribution",
			xlab="Age until death",col="steelblue",nclass=100)


###################################################
### code chunk number 39: Axn APV and stochastic
###################################################
numSim=50000
#term life insurance
APVAxn=Axn(soa08Act,x=25,n=40,type="EV")
APVAxn
sampleAxn=rLifeContingencies(n=numSim, lifecontingency="Axn",
		object=soa08Act,x=25,t=40,parallel=TRUE)
t.test(x=sampleAxn,mu=APVAxn)$p.value

#increasing life insurance

APVIAxn=IAxn(soa08Act,x=25,n=40,type="EV")
APVIAxn
sampleIAxn=rLifeContingencies(n=numSim, lifecontingency="IAxn",
		object=soa08Act,x=25,t=40,parallel=TRUE)
t.test(x=sampleIAxn,mu=APVIAxn)$p.value

#temporary annuity due

APVaxn=axn(soa08Act,x=25,n=40,type="EV")
APVaxn
sampleaxn=rLifeContingencies(n=numSim, lifecontingency="axn",
		object=soa08Act,x=25,t=40,parallel=TRUE)
t.test(x=sampleaxn,mu=APVaxn)$p.value

#endowment insurance
APVAExn=AExn(soa08Act,x=25,n=40,type="EV")
APVAExn
sampleAExn=rLifeContingencies(n=numSim, lifecontingency="AExn",
		object=soa08Act,x=25,t=40,parallel=TRUE)
t.test(x=sampleAExn,mu=APVAExn)$p.value


###################################################
### code chunk number 40: figsim
###################################################
	par(mfrow=c(2,2))
	hist(sampleAxn, main="Term insurance",xlab="actuarial present value",nclass=100, col="steelblue",freq=FALSE)
	hist(sampleIAxn, main="Increasing life insurance",xlab="actuarial present value",nclass=100, col="steelblue",freq=FALSE)
	hist(sampleaxn, main="Temporary annuity due",xlab="actuarial present value",nclass=100, col="steelblue",freq=FALSE)
	hist(sampleAExn, main="Endowment insurance",xlab="actuarial present value",nclass=100, col="steelblue",freq=FALSE)


###################################################
### code chunk number 41: stochastic example full 1
###################################################
#set the various parameters
employees=500
salaryDistribution=rlnorm(n=employees,m=10.77668944,s=0.086177696) #log-normal distribution paramenters.
ageDistribution=round(runif(n=employees,min=25, max=65))
policyLength=sapply(65-ageDistribution, min,1)
#function to obtain the type of benefit
getEmployeeBenefit<-function(index,type="EV") {
	out=numeric(1)
	out=salaryDistribution[index]*Axn(actuarialtable=soa08Act, 
			x=ageDistribution[index],n=policyLength[index], 
			i=0.02,m=0,k=1, type=type)
	return(out)
}

#configure the parallel library 
#environment
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
#determine the best estimate of employees benefit
employeeBenefits=numeric(employees)
employeeBenefits<- parSapply(cl, 1:employees,getEmployeeBenefit, type="EV")
employeeBenefit=sum(employeeBenefits)

#determine the risk margin
nsim=100 #use 100 simulations
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
#summarize results
riskMargin=as.numeric(quantile(benefitDistribution,.75)-employeeBenefit)
totalBookedCost=employeeBenefit+riskMargin

employeeBenefit
riskMargin
totalBookedCost


