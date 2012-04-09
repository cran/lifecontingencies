# TODO: Add comment
# 
# Author: Giorgio Spedicato
###############################################################################


########################computationally intense simulations and graphs###############

#distribuzione annuity 
nsim=500000
simAnnuity<-numeric(nsim)
simAnnuity<-rLifeContingencies(n=nsim, lifecontingency="axn",object=soa08Act,
		x=65,t=getOmega(soa08Act)-65, i=soa08Act@interest,m=0,k=1, parallel=TRUE)

#distribuzione morti

deathsIPS55M<-rLife(n=nsim,ips55M, x=0, type="Kx")


setwd("./images")

png("fig1.png")

	plot(x=irates, y=insurance, 
			main="Effect of interest on APV of term life insurance"
			,lty=3)

dev.off()


png("fig2.png")

	hist(simAnnuity, col="steelblue", main="Annuity distribution simulation")

dev.off()


png("fig3.png")

	plot(x=years, y=annuityRes, col="steelblue", main="Benefit reserve", ylab="amount",xlab="years")

dev.off()


png("fig4.png")

	hist(deathsIPS55M, freq=FALSE, main="IPS55M table Kx distribution",
			xlab="Age until death",col="steelblue")

dev.off()

setwd("../")

