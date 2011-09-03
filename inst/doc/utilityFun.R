# TODO: Add comment
# 
# Author: Giorgio
###############################################################################

library(lifecontingencies)
data(soa08Act)
data(demoita)

#create IPS55

lxIPS55M<-with(demoita, IPS55M)
pos2Remove<-which(lxIPS55M %in% c(0,NA))
lxIPS55M<-lxIPS55M[-pos2Remove]
xIPS55M<-seq(0,length(lxIPS55M)-1,1)

lxIPS55F<-with(demoita, IPS55F)
pos2Remove<-which(lxIPS55F %in% c(0,NA))
lxIPS55F<-lxIPS55F[-pos2Remove]
xIPS55F<-seq(0,length(lxIPS55F)-1,1)

ips55M=new("lifetable",x=xIPS55M, lx=lxIPS55M, name="IPS 55 Males")
ips55F=new("lifetable",x=xIPS55F, lx=lxIPS55F, name="IPS 55 Females")


#effetto tassi di interesse life insurance
irates=seq(from=0.01, to=0.06, by=0.0025)
insurance=numeric(length(irates))
for(i in 1:length(irates)) insurance[i]=Axn(soa08Act,40,10,irates[i])


#distribuzione annuity 
nsim=1000
simAnnuity<-numeric(nsim)
for (i in 1:nsim) simAnnuity[i]=axn(soa08Act, x=65, type="ST")


#reserve annuity

yearlyRate=12000
irate=0.02
#insured aged 25, annuity payable at 65
#montly payments
APV=yearlyRate*axn(soa08Act, x=25, i=irate,m=65-25,k=12)
levelPremium=APV/axn(soa08Act, x=25,n=65-25,k=12)
reserve<-function(t) {
	out<-NULL
	if(t<65-25) out=yearlyRate*axn(soa08Act, x=25+t, i=irate,m=65-(25+t),k=12)-levelPremium*axn(soa08Act, x=25+t,n=65-(25+t),k=12) else {
		out=yearlyRate*axn(soa08Act, x=25+t, i=irate,k=12)
	}
	return(out)
}
years=seq(from=0, to=getOmega(soa08Act)-25-1,by=1)
annuityRes=numeric(length(years))
for(i in years) annuityRes[i+1]=reserve(i)



setwd("./images")

png("Fig1.png")

	plot(x=irates, y=insurance, 
			main="Effect of interest on APV of term life insurance"
	,lty=3)

dev.off()


png("Fig2.png")

hist(simAnnuity, col="dark red", main="Annuity distribution simulation")

dev.off()


png("Fig3.png")

	plot(x=years, y=annuityRes, col="dark red", main="Benefit reserve", ylab="amount",xlab="years")

dev.off()

setwd("../")
