#using italian IPS55 life table (MALES), 2% interest rate
data(demoIta)
lxIPS55M<-with(demoita, IPS55M)
pos2Remove<-which(lxIPS55M %in% c(0,NA))
lxIPS55M<-lxIPS55M[-pos2Remove]
xIPS55M<-seq(0,length(lxIPS55M)-1,1)
IPS55MAct=new("actuarialtable",x=xIPS55M,lx=lxIPS55M,interest=0.02, name="IPS55M")

#evaluate the monthly premium for a term life insurance
#male 30 year old 35 year

Premium=(100000*Axn(IPS55MAct, x=30, n=35))/(12*axn(IPS55MAct, x=30, n=35,k=12))

for(i in 1:34)
{
	actuarial_value_of_insured_amount=100000*Axn(IPS55MAct, x=30+i, n=35-i)
	actuarial_value_of_premium_flows=12*Premium*axn(IPS55MAct, x=30+i, n=35-i,k=12)
	reserve=actuarial_value_of_insured_amount-actuarial_value_of_premium_flows
	cat("At year ",i, "the reserve is ",reserve, "\n")
}