\name{show-methods}
\docType{methods}
\alias{show-methods}
\alias{show,actuarialtable-method}
\alias{show,lifetable-method}
\title{Methods for Function \code{show}}
\description{
 Methods for function \code{show}
}
\section{Methods}{
\describe{
\item{\code{signature(object = "actuarialtable")}}{
This method plots the Survival function implied in the number at risk per age
in the life table. Following entries are reported:
}

\item{\code{signature(object = "lifetable")}}{
This method plots the Survival function implied in the number at risk per age
in the life table.
}
}}
\examples{
		#assumes SOA example life table to be load
		data(soaLt)
		soa08=with(soaLt, new("lifetable",
		x=x,lx=Ix,name="SOA2008"))
		#the last attainable age under SOA life table is
		show(soa08) 

}
\keyword{methods}

