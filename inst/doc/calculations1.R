# TODO: Add comment
# 
# Author: Giorgio
###############################################################################

library(lifecontingencies)
data(soa08Act)
data(demoIta)

#create IPS55

lxIPS55M<-with(demoIta, IPS55M)
pos2Remove<-which(lxIPS55M %in% c(0,NA))
lxIPS55M<-lxIPS55M[-pos2Remove]
xIPS55M<-seq(0,length(lxIPS55M)-1,1)

lxIPS55F<-with(demoIta, IPS55F)
pos2Remove<-which(lxIPS55F %in% c(0,NA))
lxIPS55F<-lxIPS55F[-pos2Remove]
xIPS55F<-seq(0,length(lxIPS55F)-1,1)

ips55M=new("lifetable",x=xIPS55M, lx=lxIPS55M, name="IPS 55 Males")
ips55F=new("lifetable",x=xIPS55F, lx=lxIPS55F, name="IPS 55 Females")

#effetto tassi di interesse life insurance
irates=seq(from=0.01, to=0.06, by=0.0025)
insurance=numeric(length(irates))
for(i in 1:length(irates)) insurance[i]=Axn(soa08Act,40,10,irates[i])

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


