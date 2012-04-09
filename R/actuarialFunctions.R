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
	if(type=="EV") out=presentValue(cashFlows=1, timeIds=n, 
				interestRates=interest, probabilities=prob) else if(type=="ST") out=rLifeContingencies(n=1,lifecontingency="Exn", 
				object=actuarialtable, x=x,t=n,i=actuarialtable@interest, m=1,k=1)
	#out=discount^2*prob*(1-prob)
	return(out)
}
#function to obtain the annuity
axn<-function(actuarialtable, x, n,i, m,k=1, type="EV")
{
	out<-NULL
	if(missing(actuarialtable)) stop("Error! Need an actuarial actuarialtable")
	if(missing(x)) stop("Error! Need age!")
	
	if(x>getOmega(actuarialtable)) {
		out=0
		return(out)
	}
	if(missing(m)) m=0
	if(missing(n)) n=getOmega(actuarialtable)-x-m #n=getOmega(actuarialtable)-x-m-1
	if(n==0) {
		out=0
		return(out)
	}
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
	} else if(type=="ST"){
		out=rLifeContingencies(n=1,lifecontingency="axn", 
				object=actuarialtable, x=x,t=n,i=actuarialtable@interest, m=m,k=k)
	}
	return(out)
}

#shall write the Rd file
axyn<-function(tablex, tabley, x,y, n,i, m,k=1, status="joint", type="EV")
{
	out<-NULL
	if(missing(tablex)) stop("Error! Need table for X life")
	if(missing(tabley)) stop("Error! Need table for Y life")
	if(missing(x)) stop("Error! Need age for X!")
	if(missing(y)) stop("Error! Need age for Y!")
	if(missing(m)) m=0
	if(missing(n)) n=max(getOmega(tablex)-x,getOmega(tabley)-y)-m #maximum sequence of payments
	if(tablex@interest!=tabley@interest) {
		warning("Warning! Intesters differ between tablex and tabley. Using average")
		}
	if(!missing(i)) interest=i else interest=0.5*(tablex@interest+tabley@interest) #i an interest rate is provided the provided interest rate overrides the actuarialtable interest rate
	if(any(x<0,y<0,m<0,n<0)) stop("Error! Negative parameters")
	#computation of quantities, assuming fractional payments
		payments=rep(1/k,n*k)
		probs=numeric(n*k)		
		times=m+seq(from=0, to=(n-1/k),by=1/k)
		
		for(i in 1:length(times)) probs[i]=pxyt(objectx=tablex,objecty=tabley, 
			x=x,y=y,t=times[i],status=status)
		#discounts=(1+actuarialtable@interest)^-times
		#out<-sum(payments*discounts*probs)
	if(type=="EV") {
		out<-presentValue(cashFlows=payments, timeIds=times, interestRates=interest, probabilities=probs)
	} else	if(type=="ST"){
		out=0
		for(i in 1:length(times)) out=out+1/k*rbinom(n=1, size=1, prob=probs[i])*(1+interest)^-times[i]
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
	} else if(type=="ST"){
		out=rLifeContingencies(n=1,lifecontingency="Axn", 
				object=actuarialtable, x=x,t=n,i=actuarialtable@interest, m=m,k=k)
	}
	return(out)
}


Axyn<-function(tablex, x,tabley, y, n,i, m, k=1, status="joint", type="EV")
{
	out<-NULL
	if(any(missing(tablex),missing(tabley))) stop("Error! Need tables")
	if(any(missing(x),missing(y))) stop("Error! Need ages!")
	if(k<1) stop("Error! Periods in a year shall be no less than 1")
	if(missing(m)) m=0
	if(missing(n)) n=max(getOmega(tablex)-x,getOmega(tabley)-y)-m-1

	if(tablex@interest!=tabley@interest) {
		warning("Warning! Intesters differ between tablex and tabley. Using average")
		}
	if(!missing(i)) interest=i else interest=0.5*(tablex@interest+tabley@interest) #i an interest rate is provided the provided interest rate overrides the actuarialtable interest rate
	
	if(n==0) return(0)
	if(any(x<0,y<0,m<0,n<0)) stop("Error! Negative parameters")
	
	#perform calculations
		
		payments=rep(1,n*k)
		probs=numeric(n*k)		
		times=m+seq(from=0, to=(n-1/k),by=1/k)
		for(i in 1:length(times)) probs[i]=(pxyt(objectx=tablex,objecty=tabley, x=x,y=y, status=status, t=times[i])*qxyt(objectx=tablex,objecty=tabley, 
								x=x+times[i],y=y+times[i],
								t=1/k,status=status))
		#for(i in 1:length(times)) probs[i]=(pxt(actuarialtable, x,times[i])*qxt(actuarialtable, x+times[i],1))
		discounts=(1+interest)^-(times+1/k)
	#gets outpus
	
	if(type=="EV") {
		out<-sum(payments*discounts*probs)
	} else if(type=="ST"){
		out=0
		for(i in 1:length(times)) 
		{
			out=((1+interest)^-(times[i]+1/k))*rbinom(n=1, size=1, prob=probs[i])
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

	if(missing(n)) n=getOmega(actuarialtable)-x-m #n=getOmega(actuarialtable)-x-m-1
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
	} else if(type=="ST") {
		out=rLifeContingencies(n=1,lifecontingency="IAxn", 
				object=actuarialtable, x=x,t=n,i=actuarialtable@interest, m=m,k=1) #k is not defined yet
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
	
	if(missing(n)) n=getOmega(actuarialtable)-x-m #n=getOmega(actuarialtable)-x-m-1
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
	} else if(type=="ST")
	{
		out=rLifeContingencies(n=1,lifecontingency="DAxn", 
				object=actuarialtable, x=x,t=n,i=actuarialtable@interest, m=m,k=1)
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

#n-year increasing
#recursive function
Iaxn<-function(actuarialtable, x, n,i, m=0, type="EV")
{
  out<-NULL
	if(missing(actuarialtable)) stop("Error! Need an actuarial actuarialtable")
	if(missing(m)) m=0
	if(missing(x)) stop("Error! Need age!")
	#m is set equal to zero at the moment
	if(missing(n)) n=getOmega(actuarialtable)-x-m #n=getOmega(actuarialtable)-x-m-1
	if(!missing(i)) interest=i else interest=actuarialtable@interest 
	#i an interest rate is provided the provided interest rate overrides the 
	#actuarialtable interest rate
		payments=numeric(n)
		probs=numeric(n)
		times=numeric(n)
		discounts=numeric(n)
		
		payments=seq(from=1, to=n, by=1)
		times=m+seq(from=0, to=(n-1),by=1) 
		for(i in 1:length(times)) probs[i]=pxt(actuarialtable, x,times[i])
		discounts=(1+interest)^-(times)
		out<-sum(payments*discounts*probs)
	return(out)
}



#pure endowment function
AExn<-function(actuarialtable, x, n,i, k=1, type="EV")
{
	out<-NULL
	if(missing(actuarialtable)) stop("Error! Need an actuarial actuarialtable")
	if(missing(x)) stop("Error! Need age!")
	if(k<1) stop("Error! Periods in a year shall be no less than 1")
	if(missing(n)) n=getOmega(actuarialtable)-x-1
	if(!missing(i)) interest=i else interest=actuarialtable@interest #i an interest rate is provided the provided interest rate overrides the actuarialtable interest rate
	if(n==0) return(0)
	if(any(x<0,n<0)) stop("Error! Negative parameters")
	
	if(type=="EV") {
		out<-Axn(actuarialtable=actuarialtable, x=x, n=n, i=i,m=0, k=k,type="EV")+Exn(actuarialtable=actuarialtable, x=x, n=n, i=i,
				type="EV")
	} else if(type=="ST"){
		out=rLifeContingencies(n=1,lifecontingency="AExn", 
				object=actuarialtable, x=x,t=n,i=actuarialtable@interest, k=k)
	}
	return(out)
}


#x=35
#t=30
#n=200000
#object=soa08Act
#lifecontingency="EAxn"
#i=0.06
#out<-rLifeContingencies(n=20000,lifecontingency="Axn", object=soa08Act, x=35,
#		t=30,i=0.06,k=1, parallel=TRUE)


