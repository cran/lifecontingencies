##calculates monthly installment of a loan of $100,000, interest rate 5.00%

intEff=(1+0.05)^(1/12)-1
Capital=100000
#Montly installment

R=Capital/annuity(i=intEff, n=10*12, type = "immediate")
R
balance=numeric(10*12+1)
capitals=numeric(10*12+1)
interests=numeric(10*12+1)
balance[1]=Capital
interests[1]=0
capitals[1]=0

for(i in (2:121))	{
	balance[i]=balance[i-1]*(1+intEff)-R
	interests[i]=balance[i-1]*intEff
	capitals[i]=R-interests[i]
}
loanSummary=data.frame(rate=c(0, rep(R,10*12)), balance, interests, capitals)

