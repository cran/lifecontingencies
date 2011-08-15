\name{IAxn}
\alias{IAxn}

\title{
Function to evaluate increasing life insurance
}
\description{
This function evaluate the actuarial value of capital increasing
life insurance. The amount payable at the end of year of death are: \eqn{1, 2, \ldots, n-1, n}. N can be set as \eqn{\omega-x-1}.
}
\usage{
IAxn(actuarialtable, x, n,i, m = 0, type = "EV")
}
\arguments{
  \item{actuarialtable}{
The actuarial table used to perform life - contingencies calculations.
}
  \item{x}{
The age of the insured.
}
  \item{n}{
The term of life insurance. If missing it is assumed to be whole - life.
}
\item{i}{
	Interest rate (overrides the interest rate of the actuarial table object).
}
  \item{m}{
The deferring period. If missing m=0
}
  \item{type}{
If "EV" (default) the actuarial value is returned. Otherwise a number drawn from the distribution of IAXn. 
}
}
\details{
"ST" has not been implemented yet.
}
\value{ A numeric value.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato}

\section{Warning }{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}



\seealso{
\code{\link{DAxn}}
}
\examples{

%	\dontrun{
		#assumes SOA example life table to be load
		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		#evaluate the value of a 10 years increasing term life insurance for an aged 25
		IAxn(actuarialtable=soa08Act, x=25, n=10) 
%	}

}
