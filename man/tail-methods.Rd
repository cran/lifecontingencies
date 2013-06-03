\name{tail-methods}
\docType{methods}
\alias{tail-methods}
\alias{tail,ANY-method}
\alias{tail,lifetable-method}
\title{Methods for Function \code{tail} in Package \pkg{lifecontingencies}}
\description{
It implements \code{tail} in package \pkg{lifecontingencies}. In practice it returns the last six 
rows of underlying life tables both for \code{lifetable} and for \code{actuarialtable} classes.
}
\section{Methods}{
\describe{
\item{\code{signature(x = "ANY")}}{
A \code{lifetable} object.
}

\item{\code{signature(x = "lifetable")}}{
A \code{lifetable} object.
}
}}
\examples{
		#assumes SOA example life table to be load
		data(soaLt)
		tail(soaLt) 
}
\keyword{methods}

