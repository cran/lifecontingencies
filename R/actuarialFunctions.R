# TODO: varianze funzioni + importanti

# 
# Author: Giorgio Spedicato
###############################################################################

#function to obtain the endowment
Exn<-function(actuarialtable, x, n, i,type="EV")
{
	out<-NULL
	if(missing(actuarialtable)) stop("Error! Need an actuarial actuarialtable") #request an actuarial actuarialtable
	prob=pxt(actuarialtable,x,n)
	if(!missing(i)) interest=i else interest=actuarialtable@interest #i an interest rate is provided the provided interest rate overrides the actuarialtable interest rate
	discount=(1+interest)^(-n)
	#defines the outputs
	if(type=="EV") out=presentValue(cashFlows=1, timeIds=n, interestRates=interest, probabilities=prob)
	else if(type=="ST") out=discount*rbinom(n=1,size=1,prob=prob)
	else if(type=="VR") 
		{	#using rule of moments
			m1=presentValue(cashFlows=1, timeIds=n, interestRates=interest, probabilities=prob)
			fint=interest2Intensity(interest)
			fint2=fint*2
			m2=presentValue(cashFlows=1, timeIds=n, interestRates=intensity2Interest(fint2), probabilities=prob)
			out=m2-m1^2
		}
	
	#out=discount^2*prob*(1-prob)
	return(out)
}
#function to obtain the annuity
axn<-function(actuarialtable, x, n,i, m,k=1, type="EV")
{
	out<-NULL
	if(missing(actuarialtable)) stop("Error! Need an actuarial actuarialtable")
	if(missing(x)) stop("Error! Need age!")
	if(missing(m)) m=0
	if(missing(n)) n=getOmega(actuarialtable)-x-m-1
	if(!missing(i)) interest=i else interest=actuarialtable@interest #i an interest rate is provided the provided interest rate overrides the actuarialtable interest rate
	if(any(x<0,m<0,n<0)) stop("Error! Negative parameters")
	#computation of quantities, assuming fractional payments
		payments=rep(1/k,n*k)
		probs=numeric(n*k)		
		times=m+seq(from=0, to=(n-1/k),by=1/k)
		
		for(i in 1:length(times)) probs[i]=pxt(actuarialtable, x,times[i])
		#discounts=(1+actuarialtable@interest)^-times
		#out<-sum(payments*discounts*probs)
	if(type=="EV") {
		out<-presentValue(cashFlows=payments, timeIds=times, interestRates=interest, probabilities=probs)
	}
	if(type=="ST"){
		out=0
		for(i in 1:length(times)) out=out+1/k*rbinom(n=1, size=1, prob=pxt(actuarialtable, x, times[i]))*(1+interest)^-times[i]
	}
	return(out)
}

#function to obtain the Life Insurance
#actuarialtable: an actuarial actuarialtable object
#x: beginnin life age
#m: deferring term
#type: output requested: default expected value
Axn<-function(actuarialtable, x, n,i, m, k=1, type="EV")
{
	out<-NULL
	if(missing(actuarialtable)) stop("Error! Need an actuarial actuarialtable")
	if(missing(x)) stop("Error! Need age!")
	if(k<1) stop("Error! Periods in a year shall be no less than 1")
	if(missing(m)) m=0
	if(missing(n)) n=getOmega(actuarialtable)-x-m-1
	if(!missing(i)) interest=i else interest=actuarialtable@interest #i an interest rate is provided the provided interest rate overrides the actuarialtable interest rate
	if(n==0) return(0)
	if(any(x<0,m<0,n<0)) stop("Error! Negative parameters")
	
	#perform calculations
		#payments=rep(1,n)
		#probs=numeric(n)
		#times=m+seq(from=0, to=(n-1),by=1) 
		payments=rep(1,n*k)
		probs=numeric(n*k)		
		times=m+seq(from=0, to=(n-1/k),by=1/k)
		startAge=x
		for(i in 1:length(times)) probs[i]=(pxt(object=actuarialtable, x=startAge,t=times[i])*qxt(object=actuarialtable, x=startAge+times[i],t=1/k))
		#for(i in 1:length(times)) probs[i]=(pxt(actuarialtable, x,times[i])*qxt(actuarialtable, x+times[i],1))
		discounts=(1+interest)^-(times+1/k)
	#gets outpus
	
	if(type=="EV") {
	
		out<-sum(payments*discounts*probs)
	}
	if(type=="ST"){
		out=0
		for(i in 1:length(times)) 
		{
			out=((1+interest)^-(times[i]+1/k))*rbinom(n=1, size=1, prob=pxt(actuarialtable, x,times[i])*qxt(actuarialtable,x+times[i],1/k))
			if(out>0) break
		}
	}
	return(out)
}

#n-year term whole life
#recursive function
IAxn<-function(actuarialtable, x, n,i, m=0, type="EV")
{
	out<-NULL
	if(missing(actuarialtable)) stop("Error! Need an actuarial actuarialtable")
	if(missing(m)) m=0
	if(missing(x)) stop("Error! Need age!")
	m=0 #m is set equal to zero at the moment
	if(missing(n)) n=getOmega(actuarialtable)-x-m-1
	if(!missing(i)) interest=i else interest=actuarialtable@interest #i an interest rate is provided the provided interest rate overrides the actuarialtable interest rate
	y=x+n
	
	if(type=="EV") {
		payments=seq(from=1, to=n, by=1)
		probs=numeric(n)
		times=m+seq(from=0, to=(n-1),by=1) 
		for(i in 1:length(times)) probs[i]=(pxt(actuarialtable, x,times[i])*qxt(actuarialtable, x+times[i],1))
		discounts=(1+interest)^-(times+1)
		out<-sum(payments*discounts*probs)
		return(out)
	}
	#else {
	#	if(n==0) {
	#		out=0
	#		return(out)
	#		}	else {
	#			#recursive code
	#			xplus=x+1
	#			nplus=n-1
	#			out=(qxt(actuarialtable, x,1)*(1+actuarialtable@interest)^-1)+Exn(actuarialtable, x,1)*(Axn(actuarialtable, xplus,nplus)+IAxn(actuarialtable, xplus,nplus))
	#			return(out)
	#		}
	#	}
}

#IAxn(soa08Act, 50,90,type="EV")
#IAxn(soa08Act, 50,90,type="RC")

DAxn<-function(actuarialtable, x, n,i, m=0, type="EV")
{
	out<-NULL
	if(missing(actuarialtable)) stop("Error! Need an actuarial actuarialtable")
	if(missing(x)) stop("Error! Need age!")
	if(missing(m)) m=0
	m=0 #m is set equal to zero at the moment
	if(missing(n)) n=getOmega(actuarialtable)-x-m-1
	if(!missing(i)) interest=i else interest=actuarialtable@interest #i an interest rate is provided the provided interest rate overrides the actuarialtable interest rate
	y=x+n
	if(type=="EV") {
		payments=seq(from=n, to=1, by=-1)
		probs=numeric(n)
		times=m+seq(from=0, to=(n-1),by=1) 
		for(i in 1:length(times)) probs[i]=(pxt(actuarialtable, x,times[i])*qxt(actuarialtable, x+times[i],1))
		discounts=(1+interest)^-(times+1)
		out<-sum(payments*discounts*probs)
		return(out)
	}
#	else {
#	if(n==0) {
#		out=0
#		return(out)
#		}	else {
#			#recursive code
#			xplus=x+1
#			nplus=n-1
#			out=n*Axn(actuarialtable, x,1)+Exn(actuarialtable, x,1)*DAxn(actuarialtable, xplus,nplus)
#			return(out)
#		}
#	}
}



