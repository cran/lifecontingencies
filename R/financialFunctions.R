#function to evaluate the present value of a series of cash flows

presentValue=function(cashFlows, timeIds,interestRates, probabilities)
{
	out=0
	if(missing(timeIds)) #check coherence on time id vector
	{	warning("Warning: missing time vector")
		timeIds=1
	}
	if(missing(probabilities)) #if no probabilities given than prob=1
	{
		probabilities=rep(1,length(cashFlows))
	} else {
		if(length(cashFlows)!=length(probabilities)) stop("Error! Probabilities must have same length of cash flows")
	}
	
	if(!(length(cashFlows)==length(timeIds))) stop("Error! check dimensionality of cash flow and time ids vectors") #check dimensionality of cash flows
	if((length(interestRates)>1)&(length(interestRates)!=length(timeIds))) warning("Interest rates incoherent with time ids") #check dimensioanlity of time ids
	
	interestRates=rep(interestRates,length.out=length(timeIds))
	v=(1+interestRates)^-timeIds
	out=sum((cashFlows*v)*probabilities)
	return(out)
}

#annuity function
annuity=function(i, n,m=1, type="immediate")
{
	#checks
	if(missing(i)) stop("Error! Missing interest rates") 
	if(missing(n)) stop("Error! Missing periods") 
	if(m<1) stop("Error! m must be greater or equal than 1") 
	if(is.infinite(n)) return(1/i) #perpetuity
	
	if(type=="immediate") timeIds=seq(from=1/m, to=n, by=1/m)
	else timeIds=seq(from=0, to=n-1/m, by=1/m) #due
	iRate=rep(i,length.out=n*m)
	out=presentValue(cashFlows=rep(1,length.out=n*m),interestRates = iRate, timeIds=timeIds)
	return(out)
}

#decreasing annuity
decreasingAnnuity=function(i, n)
{
	out=NULL
	if(missing(n)) stop("Error! Need number of periods")
	if(missing(i)) stop("Error! Need interest rate")
	out=(n-annuity(i=i, n=n, type="immediate"))/n
	return(out)

}
#increasing annuity
increasingAnnuity=function(i, n,type="immediate")
{
	out=NULL
	if(missing(n)) stop("Error! Need periods")
	if(missing(i)) stop("Error! Need interest rate")
	out=(annuity(i=i, n=n, type="due")-n*(1+i)^n)/i
	if(type=="due") out=out*(1+i)
	return(out)
}
accumulatedValue=function(i, periods, type="immediate")
{
	if(is.infinite(periods)) return(1/i)
	if(missing(i)) stop("Error! Missing interest rates")
	if(type=="immediate") timeIds=seq(from=1, to=periods, by=1)
	else timeIds=seq(from=0, to=periods-1, by=1) #due
	timeIds=-timeIds
	iRate=rep(i,length.out=periods)
	out=presentValue(cashFlows=rep(1,length.out=periods),i = iRate, timeIds=timeIds)
	return(out)
}
#obtain the nominal interest rate
nominal2Real=function(i, m=1, type="interest")
{
	out=NULL
	if(type=="interest") out=(1+i/m)^m-1 else 
		out=1-(1-i/m)^m
	return(out)
}
#obtain the real interest rate
real2Nominal=function(i, m=1)
{
	out=((1+i)^(1/m)-1)*m
	return(out)
}
#obtain the interest from intensity
intensity2Interest=function(intensity)
{
	out=exp(intensity*1)-1
	return(out)
}
#obtain the intensity rate from the interest rate
interest2Intensity=function(i)
{
	out=log(1+i)
	return(out)
}