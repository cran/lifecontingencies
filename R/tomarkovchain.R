# TODO: Add comment
# 
# Author: Giorgio Alfredo Spedicato
###############################################################################


#functions to convert toward a MarkovChainList

.qxToMc<-function(qx, age)
{
	statesNames=c("alive","death")
	matr=matrix(rep(0,4),nrow = 2);dimnames(matr) <- list(statesNames,statesNames)
	matr[1,1]=1-qx
	matr[1,2]=qx
	matr[2,1]=0
	matr[2,2]=1
	outMc<-new("markovchain",transitionMatrix=matr,name=age)
	return(outMc)
}

setAs("lifetable","markovchainList",
		function(from)
		{
			outChains<-list()
			ages<-seq(0,getOmega(from),1)
			for(i in ages)
			{
				ageMc<-.qxToMc(qx=qxt(from,i,1),age=as.character(i))
				outChains[[length(outChains)+1]]<-ageMc
			}
			out<-new("markovchainList",markovchains=outChains,name=from@name)
			invisible(out)
		}
	)

