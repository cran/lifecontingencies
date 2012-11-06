# TODO: function to generate Iaxn variates
# TODO: check axn
# 
# Author: Giorgio A. Spedicato
###############################################################################

##########random variables Tx and Kx generators 

#
#n=100
#object=soa08Act
#x=20
#k=2
#type="Tx"
#modified dxt to return the 1/k fraction of deaths between
#x and t

.dxtk<-function(object, x,t,k){
out=NULL
out=ifelse((x>getOmega(object)),0,dxt(object = object, 
		x=floor(x),t=t)/k)
	return(out)
}




rLife=function(n,object, x=0,k=1, type="Tx")
{
	if(missing(n)) stop("Error! Needing the n number of variates to return")
	if((class(object) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if(x>getOmega(object)) stop("Error! x > maximum attainable age")
	out=numeric(n)
	const2Add=0
	if(type=="Tx") const2Add=0.5/k # the continuous future lifetime 
	#is the curtate future lifetime + 0.5
	#determine the deaths
	omega=getOmega(object)
	#the sequence of possible death periods 
	#sequence=seq(from=0, to=omega, by=1)
	sequence=seq(from=0, to=omega+1, by=1/k)
	#dx<-sapply(sequence,dxt,object=object,t=1)
	dx<-sapply(sequence,.dxtk,object=object,t=1,k=k)
	#determine the perimeter of x
#	index=which(object@x>=x)
#	x2Sample<-object@x[index]
	
	index=which(sequence>=x)
	x2Sample<-sequence[index]
	
	
	deathsOfSample<-dx[index]
	probsOfDeath<-deathsOfSample/sum(deathsOfSample)	
	out=sample(x=x2Sample,size=n,replace=TRUE, prob=probsOfDeath)+const2Add-x
	return(out)
}


rLifexyz=function(n,tablesList,x,k=1, type="Tx")
{
	
	#initial checkings
	numTables=length(tablesList)
	if(length(x)!=numTables) stop("Error! Initial ages vector length does not match with number of lives")
	for(i in 1:numTables) {
		if(!(class(tablesList[[i]]) %in% c("lifetable", "actuarialtable"))) stop("Error! A list of lifetable objects is required")
	}	
	outVec=numeric(0)
	for(i in 1:numTables)
	{
		outI=numeric(n)
		outI=rLife(n=n,object=tablesList[[i]],x=x[i],k=k,type=type)
		outVec=c(outVec,outI)
	}	
	out=matrix(data=outVec,nrow=n,ncol=numTables,byrow=FALSE)
	return(out)
}


##get 20000 random future lifetimes for the Soa life table at birth
#data(soa08Act)
#lifes=rLife(n=1000000,object=soa08Act, x=0, type="Tx")
###check if the expected life at birth derived from the life tableis statistically equal to the expected value of the sample
##
#t.test(x=lifes, mu=exn(soa08Act, x=0, type="continuous"))
#
#lifes=rLife(n=2000000,object=soa08Act, x=0, type="Tx",k=12)
#t.test(x=lifes, mu=exn(soa08Act, x=0, type="continuous"))
#
#lifes=rLife(n=500000,object=soa08Act, x=50, type="Tx",k=12)
#t.test(x=lifes, mu=exn(soa08Act, x=50, type="continuous"))

#lifes=rLife(n=500000,object=soa08Act, x=50, type="Kx",k=1)
#t.test(x=lifes, mu=exn(soa08Act, x=50, type="curtate"))


#y=absolute policyholder's age
#T=death period
#n=length of insurance
#m=deferring period
#k=payments' frequency


#pure endowment function
.fExn<-function(T,y,n, i)
{
	out=ifelse(T<y+n,0,(1+i)^(-n))
	return(out)
}


#term life insurance function

.fAxn<-function(T,y,n, i, m, k)
{
	out=numeric(1)
	out=ifelse(((T>=y+m) && (T<=y+m+n-1/k)),(1+i)^-(T-y+1/k),0)
	return(out)
}


# x=40
# t=100
# n=100000
# object=soa08Act
# lifecontingency="Axn"
# i=0.06
# m=0
# k=12
##
# outs<-rLifeContingencies(n=n,lifecontingency=lifecontingency, object=object, x=x,t=t,i=i, 
#		m=m,k=k, parallel=TRUE)
#APV=Axn(object, x=x, k=k)
# mean(outs)
## APV
# t.test(x=outs, mu=APV)



#increasing life insurance function

.fIAxn<-function(T,y,n, i, m, k=1)
{
	out=numeric(1)
	#if policyholder dies in the insured period (y + m  --- y+m+n -1/k
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


#to be fixed!!!
.fIaxn<-function(T,y,n, i, m, k=1)
{
	out=numeric(1)
	K=T-y 
	numOfPayments=max(min(n,K+1-m),0)
	out=increasingAnnuity(i=i,n=numOfPayments,type="immediate")
	return(out)
}

#n-year endowment insurance function
.fAExn<-function(T,y,n, i, k)
{
	out=numeric(1)
	out=ifelse(((T>=y) && (T<=y+n-1/k)),(1+i)^-(T-y+1/k),(1+i)^-n)
	return(out)
}



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
	deathsTimeX=numeric(n)
	outs=numeric(n)
	#fractional payment are handled using countinuous lifetime simulation
	
	if(k==1) deathsTimeX=x+rLife(n=n,object=object,x=x,
				k=k,type="Kx") else deathsTimeX=x+rLife(n=n,object=object,
				x=x,k=k,type="Tx") #this to handle fractional payments (assume continuous...)
	
	#in case multiple life contingencies
	
	if(parallel==TRUE) {
	#set up parallel library
	require(parallel)
	type <- if (exists("mcfork", mode="function")) "FORK" else "PSOCK"
	cores <- getOption("mc.cores", detectCores())
	cl <- makeCluster(cores, type=type)
	clusterExport(cl, varlist=c("presentValue","annuity")) #richiesto da axn
	if(lifecontingency=="Axn") 
		outs=parSapply(cl=cl, deathsTimeX, .fAxn,y=x,n=t, i=i,m=m,k=k)
	else if(lifecontingency=="Exn")
		outs=parSapply(cl=cl,deathsTimeX, .fExn,y=x,n=t, i=i)
	else if(lifecontingency=="IAxn") 
		outs=parSapply(cl=cl,deathsTimeX, .fIAxn,y=x,n=t, i=i,m=m,k=k)
	else if(lifecontingency=="DAxn") 
		outs=parSapply(cl=cl,deathsTimeX, .fDAxn,y=x,n=t, i=i,m=m,k=k)
	else if(lifecontingency=="AExn") 
		outs=parSapply(cl=cl, deathsTimeX, .fAExn,y=x,n=t, i=i,k=k)
	else if(lifecontingency=="axn") 
	{
		if(missing(t)) t=getOmega(object)-x-m
		outs=parSapply(cl=cl,deathsTimeX, .faxn,y=x,n=t, i=i,m=m,k=k)
	}
	#stops the cluster
	stopCluster(cl)
	} else {
	#serial version
	if(lifecontingency=="Axn") 
		outs=sapply( deathsTimeX, .fAxn,y=x,n=t, i=i,m=m,k=k)
	else if(lifecontingency=="Exn")
		outs=sapply(deathsTimeX, .fExn,y=x,n=t, i=i)
	else if(lifecontingency=="IAxn") 
		outs=sapply(deathsTimeX, .fIAxn,y=x,n=t, i=i,m=m,k=k)
	else if(lifecontingency=="DAxn") 
		outs=sapply(deathsTimeX, .fDAxn,y=x,n=t, i=i,m=m,k=k)
	else if(lifecontingency=="AExn") 
		outs=sapply( deathsTimeX, .fAExn,y=x,n=t, i=i,k=k)
	else if(lifecontingency=="axn") 
	{
		if(missing(t)) t=getOmega(object)-x-m
		outs=sapply(deathsTimeX, .faxn,y=x,n=t, i=i,m=m,k=k)
	}
	}
	return(outs)
}

# x=60
# t=20
# n=100000
# object=soa08Act
# lifecontingency="axn"
# i=0.06
# m=0
# k=2
#
# outs<-rLifeContingencies(n=n,lifecontingency=lifecontingency, object=object, x=x,t=t,i=i, 
#		m=m,k=k, parallel=TRUE)
#APV=axn(object, x=x,n=t, k=k)
# mean(outs)
# APV
# t.test(x=outs, mu=APV)

#
#system.time(rLifeContingencies(n,lifecontingency, object, x,t,i=i, m=m,k=k, parallel=TRUE))
#system.time(rLifeContingencies(n,lifecontingency, object, x,t,i=i, m=m,k=k, parallel=FALSE))
