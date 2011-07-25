#validity method for lifetable object
setValidity("lifetable",
		function(object) {
			check<-NULL
			if(length(object@x)!=length(object@lx)) check<-"Error! x and lx does not match" #checks length of the obj
			if(any(diff(object@lx)>0)) check<-"Error! population at risk not decrementing" #check coherence of life table
			if(any(object@lx %in% c(0,NA))) {
				cat("removing NA and 0s") #removes na
				posToRemove=which(object@lx %in% c(0,NA))
				object@x=object@x[-posToRemove]
				object@lx=object@lx[-posToRemove]
			}
			if(is.null(check)) return(TRUE) else 
				return(check)
		}
)


#set method for number of deaths	
#setGeneric("dxt",function(object, x, t) standardGeneric("dxt"))
#setMethod("dxt",signature("lifetable"), function(object,x,t){
#			if(missing(x)) stop("Error! Missing x")
#			if(missing(t)) t=1
#			omega=max(object@x)+1
#			lx=object@lx[which(object@x==x)]
#			if((x+t)>=omega) out=lx else
#				out=lx-object@lx[which(object@x==t+x)]
#			return(out)
#		}
#)

dxt<-function(object, x, t) {
	#checks
	if((class(object) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Need lifetable or actuarialtable objects")
	out<-NULL
	if(missing(x)) stop("Error! Missing x")
	if(missing(t)) t=1
	omega=max(object@x)+1
	lx=object@lx[which(object@x==x)]
	if((x+t)>=omega) out=lx else
		out=lx-object@lx[which(object@x==t+x)]
	return(out)
}

#sets method for survival probability

#setGeneric("pxt", function(object,x,t) standardGeneric("pxt"))
#setMethod("pxt",signature("lifetable"),function(object,x,t)
#		{
#			if(missing(x)) stop("Missing x")
#			if(any(x<0,t<0)) stop("Check x or t domain")
#			if(missing(t)) t=1 #default 1
#			omega=max(object@x)+1 #gets omega
#			if((x+t)>=omega) out=0 else  #linearly interpolates if fractional age
#				{ if((t%%1)==0) out=object@lx[which(object@x==t+x)]/object@lx[which(object@x==x)] else {
#				  ph=object@lx[which(object@x==ceiling(t+x))]/object@lx[which(object@x==x)]
#				  pl=object@lx[which(object@x==floor(t+x))]/object@lx[which(object@x==x)]		
#				  z=t%%1
#				  out=z*ph+(1-z)*pl
#				}			
#			}
#		return(out)
#		}
#	)


pxt<-function(object, x, t)
{
	out<-NULL
	#checks
	if((class(object) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Need lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	if(any(x<0,t<0)) stop("Check x or t domain")
	if(missing(t)) t=1 #default 1
	omega=max(object@x)+1 #gets omega
	if((x+t)>=omega) out=0 else  #linearly interpolates if fractional age
	{ if((t%%1)==0) out=object@lx[which(object@x==t+x)]/object@lx[which(object@x==x)] else {
			ph=object@lx[which(object@x==ceiling(t+x))]/object@lx[which(object@x==x)]
			pl=object@lx[which(object@x==floor(t+x))]/object@lx[which(object@x==x)]		
			z=t%%1
			out=z*ph+(1-z)*pl
		}			
	}
	return(out)
}



	
#set method fo qxt
#setGeneric("qxt",function(object,x,t) standardGeneric("qxt"))
#setMethod("qxt",signature("lifetable"),function(object,x,t){
#			out=1-pxt(object=object,x=x,t=t)
#			return(out)
#		}	
#			)


#set methods of getting omega
setGeneric("getOmega", function(object) standardGeneric("getOmega"))
setMethod("getOmega","lifetable", 
		function(object) {
		out=numeric(1)
		out=max(object@x)+1
		return(out)}
				)

#getOmega<-function(object)
#{
#		out=numeric(1)
#		if((class(object) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Need lifetable or actuarialtable objects")
#		out=max(object@x)+1
#		return(out)
#}
				

qxt<-function(object, x, t)
{
	out<-NULL
	#checks
	if((class(object) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Need lifetable or actuarialtable objects")
	if(missing(x)) stop("Missing x")
	if(any(x<0,t<0)) stop("Check x or t domain")
	if(missing(t)) t=1 #default 1
	#complement of pxt
	out<-1-pxt(object=object, x=x, t=t)
	return(out)
}



#set method for exn
#setGeneric("exn", function(object, x, n) standardGeneric("exn"))
#setMethod("exn", signature("lifetable"), 
#		function(object,x,n) {
#			if(missing(x)) stop("Error! Missing x")
#			if(missing(n)) n=getOmega(object)-x
#			if(n==0) return(0)
#			probs=numeric(n)
#			for(i in 1:n) probs[i]=pxt(object,x,i)
#			out=sum(probs)
#			return(out)
#		}
#)

exn<-function(object,x,n) {
	out<-NULL
	#checks
	if((class(object) %in% c("lifetable", "actuarialtable"))==FALSE) stop("Error! Need lifetable or actuarialtable objects")
	if(missing(x)) stop("Error! Missing x")
	if(missing(n)) n=getOmega(object)-x
	if(n==0) return(0)
	probs=numeric(n)
	for(i in 1:n) probs[i]=pxt(object,x,i)
	out=sum(probs)
	return(out)
}



#show method 4 lifetable: prints x, lx, px, ex
setMethod("show","lifetable", #metodo show
		function(object){
			cat(paste("Life table",object@name),"\n")
			cat("\n")
			#get omega
			omega<-length(object@lx)+1
			#vector used to obtain px
			lxplus<-object@lx[2:length(object@lx)]
			lxplus<-c(lxplus,0)
			#ex
			lenlx=length(object@lx)
			Tx=numeric(lenlx)
			for(i in 1:lenlx) Tx[i]=sum(object@lx[i:lenlx])
			
			out<-data.frame(x=object@x, lx=object@lx,px=lxplus/object@lx, 
					ex=Tx/object@lx)
			rownames(out)=NULL
			print(out)
			cat("\n")
		}
		)
		
setMethod("show","actuarialtable", #metodo show
				function(object){
					out<-NULL
					cat(paste("Actuarial table ",object@name, "interest rate ", object@interest*100,"%"),"\n")
					cat("\n")
					#get omega
					omega<-length(object@lx)+1
					#vector used to obtain px
					lxplus<-object@lx[2:length(object@lx)]
					lxplus<-c(lxplus,0)
					#Dx
					Dx=object@lx*(1+object@interest)^(-object@x)
					lnDx=length(Dx)
					#Cx
					dx=object@lx-lxplus
					Cx=dx*(1+object@interest)^(-object@x-1)
				
					#Nx
					Nx=numeric(length(Dx))
					for(i in 1:length(Dx)) Nx[i]=sum(Dx[i:lnDx])
					#Mx
					Mx=Dx-(object@interest/(1+object@interest))*Nx
					#Rx
					Rx=numeric(length(Mx))
					lnMx=length(Mx)
					for(i in 1:length(Rx)) Rx[i]=sum(Mx[i:lnMx])
					out<-data.frame(x=object@x, lx=object@lx, Dx=Dx, Nx=Nx, Cx=Cx,
					Mx=Mx, Rx=Rx
								)					
					print(out)
					cat("\n")
				}
		)
		
setMethod("plot","lifetable",
		function(x,y,...){
			plot(x=x@x, y=x@lx, xlab="x values", 
					ylab="population at risk", 
					main=paste("life table",x@name),...)
		}
		)
#saves lifeTableObj as data frame
setAs("lifetable","data.frame",
		function(from){
			out<-data.frame(x=from@x, 
			lx=from@lx
			#,px=from@px
			#,ex=from@ex
			)
			return(out)
		}
)
#saves actuarialtable as data frame (have same slots as life - table)
setAs("actuarialtable","data.frame",
		function(from){
			out<-data.frame(x=from@x, 
			lx=from@lx
			#,ex=from@ex
			)
			return(out)
		}
)

