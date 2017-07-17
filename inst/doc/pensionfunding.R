## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
#configure
library(lifecontingencies)
data(demoIta)
lxTAB<-demoIta$SIM81
lxTAB<-lxTAB[!is.na(lxTAB) & lxTAB!=0]
xTAB<-seq(0,length(lxTAB)-1,1)
#create the table
lt=new("lifetable",x=xTAB,lx=lxTAB)

## ----cumdef, echo=TRUE---------------------------------------------------
CUM<-function(acttableAccPeriod,x,beta,i = actuarialtable@interest,j,t,
              k=1,payment="advance",acttablePaymPeriod,i2,delta=0){

    out <- numeric(1)
    if (missing(acttableAccPeriod)) 
      stop("Error! Need an actuarial actuarialtable")
    if (missing(acttablePaymPeriod)) 
      acttablePaymPeriod=acttableAccPeriod
    if(missing(i2))
      i2=i
    if (missing(x)) 
      stop("Error! Need age!")
    if (missing(beta)) 
      stop("Error! Retirement age!")
    if (x > getOmega(acttableAccPeriod)) {
      out = 0
      return(out)
    }
    if (missing(t)) 
      stop("Error! Need t")
   if (missing(j)) 
    stop("Error! Need average salary increase rate")
  if (any(x < 0, beta < 0, t < 0)) 
      stop("Error! Negative parameters")
    out=sapply(seq(x,beta-1,1),function(h)Exn(acttableAccPeriod,h,beta-h,i=i)*((1/t)+(h-x)/t*(j/(1+j)))*axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1))
    return(out)
  }

## ----cummr, echo=TRUE----------------------------------------------------
CUMmr<-function(acttableAccPeriod,x,beta,i = actuarialtable@interest,j,t,k=1,payment="advance",acttablePaymPeriod,i2,delta=0){
  
  out <- numeric(1)
  if (missing(acttableAccPeriod)) 
    stop("Error! Need an actuarial actuarialtable")
  if (missing(acttablePaymPeriod)) 
    acttablePaymPeriod=acttableAccPeriod
  if(missing(i2))
    i2=i
  if (missing(x)) 
    stop("Error! Need age!")
  if (missing(beta)) 
    stop("Error! Retirement age!")
  if (x > getOmega(acttableAccPeriod)) {
    out = 0
    return(out)
  }
  if (missing(t)) 
    stop("Error! Need t")
  if (missing(j)) 
    stop("Error! Need average salary increase rate")
  if (any(x < 0, beta < 0, t < 0)) 
    stop("Error! Negative parameters")
  out=c(sapply(seq(x,beta,1),function(h)Exn(acttableAccPeriod,h,beta-h,i=i)*((h-x)/t*(1+j)^(h-x-1))*axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1)),sapply(seq(beta+1,getOmega(acttablePaymPeriod)+1,1),function(h)((beta-x)/t*(1+j)^(beta-x-1))*(1+delta)^(h-beta)*axn(acttablePaymPeriod,h,i = (1+i2)/(1+delta)-1,k=1)))
  return(out)
}

## ----pumdef, echo=TRUE---------------------------------------------------
#Projected Unit Method
PUM<-function(acttableAccPeriod,x,beta,i = actuarialtable@interest,j,t,
              k=1,payment="advance",acttablePaymPeriod,i2,delta=0){
  
  out <- numeric(1)
  if (missing(acttableAccPeriod)) 
    stop("Error! Need an actuarial actuarialtable")
  if (missing(acttablePaymPeriod)) 
    acttablePaymPeriod=acttableAccPeriod
  if(missing(i2))
    i2=i
  if (missing(x)) 
    stop("Error! Need age!")
  if (missing(beta)) 
    stop("Error! Retirement age!")
  if (x > getOmega(acttableAccPeriod)) {
    out = 0
    return(out)
  }
  if (missing(t)) 
    stop("Error! Need t")
  if (missing(j)) 
    stop("Error! Need average salary increase rate")
  if (any(x < 0, beta < 0, t < 0)) 
    stop("Error! Negative parameters")
  out=sapply(seq(x,beta-1,1),function(h)Exn(acttableAccPeriod,h,beta-h,i=i)*1/t*
    axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1)*(1+j)^(beta-h-1))
  return(out)
}

## ----res, echo=TRUE------------------------------------------------------
PUMmr<-function(acttableAccPeriod,x,beta,i = actuarialtable@interest,j,t,k=1,payment="advance",acttablePaymPeriod,i2,delta=0){
  
  out <- numeric(1)
  if (missing(acttableAccPeriod)) 
    stop("Error! Need an actuarial actuarialtable")
  if (missing(acttablePaymPeriod)) 
    acttablePaymPeriod=acttableAccPeriod
  if(missing(i2))
    i2=i
  if (missing(x)) 
    stop("Error! Need age!")
  if (missing(beta)) 
    stop("Error! Retirement age!")
  if (x > getOmega(acttableAccPeriod)) {
    out = 0
    return(out)
  }
  if (missing(t)) 
    stop("Error! Need t")
  if (missing(j)) 
    stop("Error! Need average salary increase rate")
  if (any(x < 0, beta < 0, t < 0)) 
    stop("Error! Negative parameters")
  out=c(sapply(seq(x,beta,1),function(h)Exn(acttableAccPeriod,h,beta-h,i=i)*((h-x)/t*(1+j)^(beta-x-1))*axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1)),sapply(seq(beta+1,getOmega(acttablePaymPeriod)+1,1),function(h)((beta-x)/t*(1+j)^(beta-x-1))*(1+delta)^(h-beta)*axn(acttablePaymPeriod,h,i = (1+i2)/(1+delta)-1,k=1)))
  return(out)
}

## ----ieamdef-------------------------------------------------------------
IEAM<-function(acttableAccPeriod,x,beta,i = actuarialtable@interest,j,t,
               k=1,payment="advance",acttablePaymPeriod,i2,delta=0,type=0){

  out <- numeric(1)
  if (missing(acttableAccPeriod)) 
    stop("Error! Need an actuarial actuarialtable")
  if (missing(acttablePaymPeriod)) 
    acttablePaymPeriod=acttableAccPeriod
  if(missing(i2))
    i2=i
  if (missing(x)) 
    stop("Error! Need age!")
  if (missing(beta)) 
    stop("Error! Retirement age!")
  if (x > getOmega(acttableAccPeriod)) {
    out = 0
    return(out)
  }
  if (missing(t)) 
    stop("Error! Need t")
  if (missing(j)) 
    stop("Error! Need average salary increase rate")
  if (any(x < 0, beta < 0, t < 0)) 
    stop("Error! Negative parameters")
  if(type==0){
  out=(Exn(acttableAccPeriod,x,beta-x,i=i)*(beta-x)/t*axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1)*(1+j)^(beta-x-1))/(axn(acttablePaymPeriod,x,beta-x,i = (1+i)/(1+j)-1,k=1,payment="advance"))
  }
  else{
    out=((Exn(acttableAccPeriod,x,beta-x,i=i)*(beta-x)/t*axn(acttablePaymPeriod,beta,i = (1+i2)/(1+delta)-1,k=1)*(1+j)^(beta-x-1))/(axn(acttablePaymPeriod,x,beta-x,i ,k=1,payment="advance")))/(1+j)^seq(0,beta-x-1,1)
  }
  return(out)
}

## ----assumptions---------------------------------------------------------
#Current Unit Method
beta=65 # Beta Retirement age
x=25 #x Age of the insured.
i=0.08 # Interest Rate
t=60 #1/t is the % of the salary, recognized as retirement pension, for each year of service
j=0.06 #  average salary increases (for both growth in wages and promotional salary for seniority)
delta=0.03 #Increase of retirement pension

## ----calcandshow---------------------------------------------------------
CUM(lt,x,beta,i,j,t,k,delta=0.03)
PUM(lt,x,beta,i,j,t,k,delta=0.03)
IEAM(lt,x,beta,i,j,t,k,delta=0.03,type=0)
plot(seq(x,beta-1,1),CUM(lt,x,beta,i,j,t,k,delta=0.03),xlab="age",ylab="Contribution rate")
lines(seq(x,beta-1,1),PUM(lt,x,beta,i,j,t,k,delta=0.03),type="p",col="red")
lines(seq(x,beta-1,1),rep(IEAM(lt,x,beta,i,j,t,k,delta=0.03),beta-x),type="p",col="blue")

CUMmr(lt,x,beta,i,j,t,k,delta=0.03)
PUMmr(lt,x,beta,i,j,t,k,delta=0.03)
plot(seq(x,getOmega(lt)+1,1),CUMmr(lt,x,beta,i,j,t,k,delta=0.03),xlab="age",ylab="Mathematical reserve")
lines(seq(x,getOmega(lt)+1,1),PUMmr(lt,x,beta,i,j,t,k,delta=0.03),type="p",col="red")

