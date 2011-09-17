# TODO: Add comment
# 
# Author: Giorgio Spedicato
###############################################################################


########################computationally intense simulations and graphs###############

#distribuzione annuity 
nsim=1000
simAnnuity<-numeric(nsim)
for (i in 1:nsim) simAnnuity[i]=axn(soa08Act, x=65, type="ST")


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

