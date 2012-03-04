# TODO: Add comment
# 
# Author: Giorgio A. Spedicato
###############################################################################

##########random variables Tx and Kx generators 


rLife=function(n,object, x=0,k=1, type="Tx")
{
	if(missing(n)) stop("Error! Needing the n number of variates to return")
	if((class(object) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if(x>getOmega(object)) stop("Error! x > maximum attainable age")
	out=numeric(n)
	const2Add=0
	if(type=="Tx") const2Add=0.5/k # the continuous future lifetime is the curtate future lifetime + 0.5
	#determine the deaths
	omega=max(object@x)
	sequence=seq(from=0, to=omega, by=1/k) #the sequence of possible death periods 
	dx<-sapply(sequence,dxt,object=object,t=1)
	#determine the perimeter of x
	index=which(object@x>=x)
	x2Sample<-object@x[index]
	deathsOfSample<-dx[index]
	probsOfDeath<-deathsOfSample/sum(deathsOfSample)	
	out=sample(x=x2Sample,size=n,replace=TRUE, prob=probsOfDeath)+const2Add-x
	return(out)
}


##get 20000 random future lifetimes for the Soa life table at birth
#data(soa08Act)
#lifes=rLife(n=20000,object=soa08Act, x=0, type="Tx")
##check if the expected life at birth derived from the life table
##is statistically equal to the expected value of the sample
#
#t.test(x=lifes, mu=exn(soa08Act, x=0, type="continuous"))



#pure endowment function
.fExn<-function(T,x,n, i)
{
	out=0*(T<x+n)+(T>=x+n)*(1+i)^(-n)
	return(out)
}


#term life insurance function

.fAxn<-function(T,x,n, i, m, k)
{
	out=numeric(1)
	out=ifelse(((T>=x+m) && (T<=x+m+n-1/k)),(1+i)^-(T-x+1/k),0)
	return(out)
}

#increasing life insurance function

.fIAxn<-function(T,x,n, i, m, k=1)
{
	out=numeric(1)
	out=ifelse(((T>=x+m) && (T<=x+m+n-1/k)),(T-(x+m)+1/k)*(1+i)^-(T-x+1/k),0)
	return(out)
}


#decreasing life insurance function

.fDAxn<-function(T,x,n, i, m, k=1)
{
	out=numeric(1)
	out=ifelse(((T>=x+m) && (T<=x+m+n-1/k)),(n-(T-(x+m)+1/k))*(1+i)^-(T-x+1/k),0)
	return(out)
}


#annuity example: DOES NOT WORK

.faxn<-function(T,x,n, i, m, k=1)
{
	out=numeric(1)
	numOfPayments=min(n,T-x-m)
	if(numOfPayments==0) return(0)
	timeIds=seq(from=m, to=numOfPayments-1/k, by=1/k) #due
	iRate=rep(i,length.out=numOfPayments*k)
	result=presentValue(cashFlows=rep(1/k,length.out=numOfPayments*k),interestRates = iRate, 
			timeIds=timeIds)
	out=ifelse((T>=x+m),result,0)
	return(out)
}



rLifeContingencies<-function(n,lifecontingency, object, x,t,i=object@interest, m=0,k=1)
{
	deathsTime=numeric(n)
	outs=numeric(n)
	deathsTime=x+rLife(n=n,object=object,x=x,k=k,type="Kx")
	if(lifecontingency=="Axn") 
		outs=sapply(deathsTime, .fAxn,x=x,n=t, i=i,m=m,k=k)
	else if(lifecontingency=="Exn")
		outs=sapply(deathsTime, .fExn,x=x,n=t, i=i)
	else if(lifecontingency=="IAxn") 
		outs=sapply(deathsTime, .fIAxn,x=x,n=t, i=i,m=m,k=1)
	else if(lifecontingency=="DAxn") 
		outs=sapply(deathsTime, .fIAxn,x=x,n=t, i=i,m=m,k=1)
	else if(lifecontingency=="axn") 
		outs=sapply(deathsTime, .faxn,x=x,n=t, i=i,m=m,k=1)
	else outs=NULL
	return(outs)
}

#n=25000

##test .rAxn
#1

#object=soa08Act
#i=soa08Act@interest

#x=40
#t=25
#k=1
#m=4
#
#APVTest=Axn(actuarialtable=object, x=40,n=t, i=i,m=m,k=k)
#outs=.rLifeContingencies(n=n,lifecontingency="Axn", object=soa08Act, x=x,t=t,m=m,k=k)
#
#APVTest
#mean(outs)
#t.test ok
#t.test(x=outs, mu=APVTest)


#test .fIAxn
#2
##
#object=soa08Act
#i=soa08Act@interest
##
#x=40
#t=25
#k=1
#m=0
#
#APVTest=IAxn(actuarialtable=object, x=40,n=t, i=i,m=m)
#outs=.rLifeContingencies(n=n,lifecontingency="IAxn", object=soa08Act, x=x,t=t,m=m,k=k)
#
#APVTest
#mean(outs)
##t.test ok
#t.test(x=outs, mu=APVTest)
#
##test .fDAxn
##3
##
#object=soa08Act
#i=soa08Act@interest
##
#x=40
#t=25
#k=1
#m=0
#
#APVTest=IAxn(actuarialtable=object, x=40,n=t, i=i,m=m)
#outs=.rLifeContingencies(n=n,lifecontingency="DAxn", object=soa08Act, x=x,t=t,m=m,k=k)
#
#APVTest
#mean(outs)
##t.test ok
#t.test(x=outs, mu=APVTest)


##test .faxn
##3
###
#object=soa08Act
#i=soa08Act@interest
#
#x=40
#t=25
#k=1
#m=0
##
#APVTest=axn(actuarialtable=object, x=40,n=t, i=i,m=m)
#outs=.rLifeContingencies(n=n,lifecontingency="axn", object=soa08Act, x=x,t=t,m=m,k=k)
##
#APVTest
#summary(outs)
###t.test 
#t.test(x=outs, mu=APVTest)
#test2=numeric(n)
#for(i in 1:n) {test2[i]=axn(actuarialtable=object, x=40,n=t, i=i,
#			m=m,type="ST")
#
#}
