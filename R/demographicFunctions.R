

dxt<-function(object, x, t) {
	#checks
	if((class(object) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Need lifetable or actuarialtable objects")
	out<-NULL
	if(missing(x)) stop("Error! Missing x")
	if(missing(t)) t=1
	omega=getOmega(object) #prima object+1
	lx=object@lx[which(object@x==x)]
	if((x+t)>omega) out=lx else #before >=
		out=lx-object@lx[which(object@x==t+x)]
	return(out)
}


pxt<-function(object, x, t, fractional="linear")
{
	out<-NULL
	#checks
	if((class(object) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Need lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	if(any(x<0,t<0)) stop("Check x or t domain")
	if(missing(t)) t=1 #default 1
	omega=getOmega(object)
	#if the starting age is fractional apply probability laws
	if((x-floor(x))>0) {
		integerAge=floor(x)
		excess=x-floor(x)
		out=pxt(object=object, x=integerAge,t=excess+t)/pxt(object=object, x=integerAge,t=excess)
		return(out)
	} #before x+t>=omega
	if((x+t)>omega) out=0 else  #fractional ages
	{ if((t%%1)==0) out=object@lx[which(object@x==t+x)]/object@lx[which(object@x==x)] else {
			z=t%%1 #the fraction of year

			#linearly interpolates if fractional age
		if(fractional=="linear"){
			ph=object@lx[which(object@x==ceiling(t+x))]/object@lx[which(object@x==x)]
			pl=object@lx[which(object@x==floor(t+x))]/object@lx[which(object@x==x)]		
			out=z*ph+(1-z)*pl
			} else if(fractional=="constant force") {
				out=pxt(object=object, x=x,t=1)^z
			} else if(fractional=="hyperbolic") {
				out=pxt(object=object, x=x,t=1)/(1-(1-z)*qxt(object=object, x=x,t=1))
			}
		}			
	}
	return(out)
}



qxt<-function(object, x, t, fractional="linear")
{
	out<-NULL
	#checks
	if((class(object) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Need lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	if(any(x<0,t<0)) stop("Check x or t domain")
	if(missing(t)) t=1 #default 1
	#complement of pxt
	out<-1-pxt(object=object, x=x, t=t, fractional=fractional)
	return(out)
}


exn<-function(object,x,n) {
	out<-NULL
	#checks
	if((class(object) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Need lifetable or actuarialtable objects")
	if(missing(x)) stop("Error! Missing x")
	if(missing(n)) n=getOmega(object)-x +1 #to avoid errors
	if(n==0) return(0)
	probs=numeric(n)
	for(i in 1:n) probs[i]=pxt(object,x,i)
	out=sum(probs)
	return(out)
}

##################two life ###########


pxyt<-function(objectx, objecty,x,y,t, status="joint")
{
	out<-NULL
	#checks
	if((class(objectx) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if((class(objecty) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	if(missing(y)) stop("Missing y")
	if(missing(t)) t=1 #default 1
	if(any(x<0,y<0,t<0)) stop("Check x, y and t domain")
	#joint survival status
	
	pxy=pxt(objectx, x,t)*pxt(objecty,y,t)
	if(status=="joint") out=pxy else out=pxt(objectx, x,t)+pxt(objecty,y,t)-pxy 
	return(out)
}

qxyt<-function(objectx, objecty,x,y,t, status="joint")
{
	out<-NULL
	#checks
	if((class(objectx) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if((class(objecty) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	if(missing(y)) stop("Missing y")
	if(missing(t)) t=1 #default 1
	if(any(x<0,y<0,t<0)) stop("Check x, y and t domain")
	out=1-pxyt(objectx=objectx, objecty=objecty,x=x,y=y,t=t, status=status)
	return(out)
}

#to check

exyt<-function(objectx, objecty,x,y,t,status="joint")
{
	out<-NULL
	#checks
	if((class(objectx) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if((class(objecty) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Objectx needs be lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	if(missing(y)) stop("Missing y")
	maxTime=max(getOmega(objectx)-x, getOmega(objecty)-y)  #maximum number of years people can live togeter
	if(missing(t)) t=maxTime
	if(any(x<0,y<0,t<0)) stop("Check x, y and t domain")
	toSum=min(t,maxTime) #max number of years to sum
	times=1:toSum
	probs=numeric(length(times))
	#out=1-pxyt(objectx=objectx, objecty=objecty,x=x,y=y,t=t, status=status)
	for(i in 1:length(times)) probs[i]=pxyt(objectx=objectx, objecty=objecty,x=x,y=y,t=times[i], status=status)
	out=sum(probs)
	return(out)
}