# TODO: function to generate Iaxn variates
# TODO: check axn
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
.fExn<-function(T,y,n, i)
{
	out=0*(T<y+n)+(T>=y+n)*(1+i)^(-n)
	return(out)
}


#term life insurance function

.fAxn<-function(T,y,n, i, m, k)
{
	out=numeric(1)
	out=ifelse(((T>=y+m) && (T<=y+m+n-1/k)),(1+i)^-(T-y+1/k),0)
	return(out)
}

#increasing life insurance function

.fIAxn<-function(T,y,n, i, m, k=1)
{
	out=numeric(1)
	out=ifelse(((T>=y+m) && (T<=y+m+n-1/k)),(T-(y+m)+1/k)*(1+i)^-(T-y+1/k),0)
	return(out)
}


#decreasing life insurance function

.fDAxn<-function(T,y,n, i, m, k=1)
{
	out=numeric(1)
	out=ifelse(((T>=y+m) && (T<=y+m+n-1/k)),(n-(T-(y+m)+1/k))*(1+i)^-(T-y+1/k),0)
	return(out)
}


#annuity example: DOES NOT WORK
# y = actual age
# Age of death
# n number of payments

.faxn<-function(T,y,n, i, m, k=1)
{
	out=numeric(1)
	K=T-y #time to death
	numOfPayments=max(min(n,K+1-m),0)
	#se 0 allora fa almeno un pagamento
#	if(numOfPayments==0) return(1)
#	if(numOfPayments<0) return(0)
#	timeIds=seq(from=0, to=numOfPayments-1/k, by=1/k)+m
#	iRate=rep(i,length.out=numOfPayments*k)
#	result=presentValue(cashFlows=rep(1/k,length.out=numOfPayments*k),interestRates = iRate, 
#			timeIds=timeIds)
	out=annuity(i=i,n=numOfPayments,k=k,m=m,type="due")
	#out=ifelse(K>=m,result,0) #if dies before start coverage
	return(out)
}


#n-year endowment insurance function
.fAExn<-function(T,y,n, i, k)
{
	out=numeric(1)
	out=ifelse(((T>=y) && (T<=y+n-1/k)),(1+i)^-(T-y+1/k),(1+i)^-n)
	return(out)
}



##serial version
#rLifeContingenciesS<-function(n,lifecontingency, object, x,t,i=object@interest, m=0,k=1)
#{
#	deathsTime=numeric(n)
#	outs=numeric(n)
#	deathsTime=x+rLife(n=n,object=object,x=x,k=k,type="Kx")
#	if(lifecontingency=="Axn") 
#		outs=sapply(deathsTime, .fAxn,y=x,n=t, i=i,m=m,k=k)
#	else if(lifecontingency=="Exn")
#		outs=sapply(deathsTime, .fExn,y=x,n=t, i=i)
#	else if(lifecontingency=="IAxn") 
#		outs=sapply(deathsTime, .fIAxn,y=x,n=t, i=i,m=m,k=1)
#	else if(lifecontingency=="DAxn") 
#		outs=sapply(deathsTime, .fDAxn,y=x,n=t, i=i,m=m,k=1)
#	else if(lifecontingency=="axn") 
#		outs=sapply(deathsTime, .faxn,y=x,n=t, i=i,m=m,k=1)
#	else outs=NULL
#	return(outs)
#}

#x=40
#t=10
#n=5000
#object=soa08Act
#lifecontingency="Axn"
#i=0.06
#m=0
#k=1


rLifeContingencies<-function(n,lifecontingency, object, x,t,i=object@interest, m=0,k=1, parallel=FALSE)
{
	deathsTime=numeric(n)
	outs=numeric(n)
	deathsTime=x+rLife(n=n,object=object,x=x,k=k,type="Kx")
	if(parallel==TRUE) {
	#set up parallel library
	require(parallel)
	type <- if (exists("mcfork", mode="function")) "FORK" else "PSOCK"
	cores <- getOption("mc.cores", detectCores())
	cl <- makeCluster(cores, type=type)
	clusterExport(cl, varlist=c("presentValue","annuity")) #richiesto da axn
	if(lifecontingency=="Axn") 
		outs=parSapply(cl=cl, deathsTime, .fAxn,y=x,n=t, i=i,m=m,k=k)
	else if(lifecontingency=="Exn")
		outs=parSapply(cl=cl,deathsTime, .fExn,y=x,n=t, i=i)
	else if(lifecontingency=="IAxn") 
		outs=parSapply(cl=cl,deathsTime, .fIAxn,y=x,n=t, i=i,m=m,k=k)
	else if(lifecontingency=="DAxn") 
		outs=parSapply(cl=cl,deathsTime, .fDAxn,y=x,n=t, i=i,m=m,k=k)
	else if(lifecontingency=="AExn") 
		outs=parSapply(cl=cl, deathsTime, .fAExn,y=x,n=t, i=i,k=k)
	else if(lifecontingency=="axn") 
	{
		if(missing(t)) t=getOmega(object)-x-m
		if(k>1) {
			warning("Warning! Stochastic modelling of fractional payments not implemented yet!")
			k=1
		}
		outs=parSapply(cl=cl,deathsTime, .faxn,y=x,n=t, i=i,m=m,k=k)
	}
	#stops the cluster
	stopCluster(cl)
	} else {
	#serial version
	if(lifecontingency=="Axn") 
		outs=sapply( deathsTime, .fAxn,y=x,n=t, i=i,m=m,k=k)
	else if(lifecontingency=="Exn")
		outs=sapply(deathsTime, .fExn,y=x,n=t, i=i)
	else if(lifecontingency=="IAxn") 
		outs=sapply(deathsTime, .fIAxn,y=x,n=t, i=i,m=m,k=k)
	else if(lifecontingency=="DAxn") 
		outs=sapply(deathsTime, .fDAxn,y=x,n=t, i=i,m=m,k=k)
	else if(lifecontingency=="AExn") 
		outs=sapply( deathsTime, .fAExn,y=x,n=t, i=i,k=k)
	else if(lifecontingency=="axn") 
	{
		if(missing(t)) t=getOmega(object)-x-m
		outs=sapply(deathsTime, .faxn,y=x,n=t, i=i,m=m,k=k)
	} 
	}
	return(outs)
}

#x=40
#t=10
#n=20000
#object=soa08Act
#lifecontingency="Axn"
#i=0.06
#m=0
#k=1
#
#outs<-rLifeContingencies(n,lifecontingency, object, x,t,i=i, m=m,k=k, parallel=TRUE)

#
#system.time(rLifeContingencies(n,lifecontingency, object, x,t,i=i, m=m,k=k, parallel=TRUE))
#system.time(rLifeContingencies(n,lifecontingency, object, x,t,i=i, m=m,k=k, parallel=FALSE))
