# TODO: Add comment
# 
# Author: Packard Bell
###############################################################################

setClass("lifetable", #classe lifetable
		representation(x="numeric",lx="numeric",name="character"),
		prototype(x=c(0,1,2,3),
				lx=c(100,90,50,10),
				name="GenericLifeTable"
					)
		)
#actuarial classes
setClass("actuarialtable",
		contains="lifetable",
		representation=representation(
			interest="numeric")
		)
