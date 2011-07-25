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
annuity=function(interestRates, periods, type="immediate")
{
	#checks
	if(missing(interestRates)) stop("Error! Missing interest rates") 
	if(missing(periods)) stop("Error! Missing periods") 
	
	if(is.infinite(periods)) return(1/interestRates) #perpetuity
	
	if(type=="immediate") timeIds=seq(from=1, to=periods, by=1)
	else timeIds=seq(from=0, to=periods-1, by=1) #due
	iRate=rep(interestRates,length.out=periods)
	out=presentValue(cashFlows=rep(1,length.out=periods),interestRates = iRate, timeIds=timeIds)
	return(out)
}

#decreasing annuity
decreasingAnnuity=function(interestRate, periods)
{
	out=NULL
	if(missing(periods)) stop("Error! Need periods")
	if(missing(interestRate)) stop("Error! Need interest rate")
	out=(periods-annuity(interestRates=interestRate, periods=periods, type="immediate"))/periods
	return(out)

}
#increasing annuity
increasingAnnuity=function(interestRate, periods,type="immediate")
{
	out=NULL
	if(missing(periods)) stop("Error! Need periods")
	if(missing(interestRate)) stop("Error! Need interest rate")
	out=(annuity(interestRates=interestRate, periods=periods, type="due")-periods*(1+interestRate)^periods)/interestRate
	if(type=="due") out=out*(1+interestRate)
	return(out)
}
accumulatedValue=function(interestRates, periods, type="immediate")
{
	if(is.infinite(periods)) return(1/interestRates)
	if(missing(interestRates)) stop("Error! Missing interest rates")
	if(type=="immediate") timeIds=seq(from=1, to=periods, by=1)
	else timeIds=seq(from=0, to=periods-1, by=1) #due
	timeIds=-timeIds
	iRate=rep(interestRates,length.out=periods)
	out=presentValue(cashFlows=rep(1,length.out=periods),interestRates = iRate, timeIds=timeIds)
	return(out)
}
#obtain the nominal interest rate
nominal2Real=function(interestRate, periods=1)
{
	out=(1+interestRate/periods)^periods-1
	return(out)
}
#obtain the real interest rate
real2Nominal=function(interestRate, periods=1)
{
	out=((1+interestRate)^(1/periods)-1)*periods
	return(out)
}
#obtain the interest from intensity
intensity2Interest=function(intensity)
{
	out=exp(intensity*1)-1
	return(out)
}
#obtain the intensity rate from the interest rate
interest2Intensity=function(interest)
{
	out=log(1+interest)
	return(out)
}