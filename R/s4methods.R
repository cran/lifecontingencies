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



#set methods of getting omega
setGeneric("getOmega", function(object) standardGeneric("getOmega"))
setMethod("getOmega","lifetable", 
		function(object) {
		out=numeric(1)
#		out=max(object@x)+1
		out=max(object@x)
		return(out)}
				)
#function to create lifetable cols
.createLifeTableCols<-function(object)
{
	omega<-length(object@lx)+1
	#vector used to obtain px
	lxplus<-object@lx[2:length(object@lx)]
	lxplus<-c(lxplus,0)
	#ex
	lenlx=length(object@lx)
	Tx=numeric(lenlx)
#	for(i in 1:lenlx) Tx[i]=sum(object@lx[i:lenlx])
	for(i in 1:lenlx) Tx[i]=sum(object@lx[i:lenlx])
	
	out<-data.frame(x=object@x, lx=object@lx,px=lxplus/object@lx, 
			ex=Tx/object@lx)
	rownames(out)=NULL
	return(out)
}

#show method 4 lifetable: prints x, lx, px, ex
setMethod("show","lifetable", #metodo show
		function(object){
			cat(paste("Life table",object@name),"\n")
			cat("\n")

			out<-.createLifeTableCols(object)
			print(out)
			cat("\n")
		}
		)
#internal function to create the actuarial table object
.createActuarialTableCols<-function(object)
{
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
			Mx=Mx, Rx=Rx)
	rownames(out)=NULL
	return(out)
	
}
		
setMethod("show","actuarialtable", #metodo show
				function(object){
					out<-NULL
					cat(paste("Actuarial table ",object@name, "interest rate ", object@interest*100,"%"),"\n")
					cat("\n")
					#create the actuarial table object
					out<-.createActuarialTableCols(object=object)
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
			out<-.createLifeTableCols(object=from)
			return(out)
		}
)
#saves actuarialtable as data frame (have same slots as life - table)
setAs("actuarialtable","data.frame",
		function(from){
			out<-.createActuarialTableCols(object=from)
			return(out)
		}
)

