\name{DAxn}
\alias{DAxn}
	
\title{
	Decreasing life insurance.
}
\description{
	This function evaluate the n-year term  decreasing life insurance. Both actuarial value and
	stochastic value can be obtained.
}
\usage{
	DAxn(actuarialtable, x, n, i,m = 0, type = "EV")
}

\arguments{
  \item{actuarialtable}{
	An actuarial table object.
}
  \item{x}{
	Age of the insured.
}
  \item{n}{
	Insurance periods length.
}
  \item{i}{
	Interest rate (overrides the interest rate of the actuarial table object).
}
  \item{m}{
	Postponement period, even fractional, if missing assumed to be 1.
}
  \item{type}{
	A string, eithed "EV" (default value) or "ST".
}

}
\details{
	Fractional periods have not been implemented yet.
}
\value{
	A numeric value representing the expected value or the simulated value.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
	Giorgio A. Spedicato
}
\note{
	Stochastic calculations have not been implemented yet in case of payments at the end of the month of death.
}


\section{Warning }{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
	losses arising from direct or indirect use of this software.
}


\seealso{
\code{\link{Axn}}, \code{\link{IAxn}}
}
\examples{

		#assumes SOA example life table to be load
		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		#evaluate the value of a 10 years decreasing term life insurance for an aged 25
		DAxn(actuarialtable=soa08Act, x=25, n=10) 

}

