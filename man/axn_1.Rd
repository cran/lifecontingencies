\name{Axn}
\alias{Axn}

\title{
	Function to evaluate life insurance.
}
\description{
	This function evaluates n-year term life insurance and whole life.
}
\usage{
	Axn(actuarialtable, x, n, i,m, k=1, type = "EV")
}

\arguments{
  \item{actuarialtable}{
	An actuarial table object.
}
  \item{x}{
	Age of the insured.
}
  \item{n}{
	Duration of insurance, if missing the insurance is considered whole life \eqn{n=\omega-x-m-1}
}
  \item{i}{
	Interest rate (overrides the interest rate of the actuarial table object).
}
  \item{m}{
	Deferring period, even fractional, if missing assumed to be 0.
}
  \item{k}{
	Number of periods per year, default=1.
}
  \item{type}{
	A string, either "EV" (default value) or "ST".
}
}
\details{
	Neither fractional periods nor the variance calculation have been implemented yet.
}
\value{
	A numeric value representing either the actuarial value of the coverage (type="EV") or 
	a number drawn from the stochastic distribution of Axn.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
	Giorgio A. Spedicato
}
\note{
	The accuracy of stochastic simulation is questionable. It will be checked with details in 
	successive releases of the package.
}



\section{Warning }{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
	\code{\link{axn}}, \code{\link{Exn}}
}
\examples{
	
		#assumes SOA example life table to be load
		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		#evaluate the value of a 40 years term life insurance for an aged 25
		Axn(actuarialtable=soa08Act, x=25, n=40) 
		#check an actuarial relevant relationship
		k=12
		i=0.06
		j=k*((1+i)^(1/k)-1)
		Axn(soa08Act, 30,k=12)
		i/j*Axn(soa08Act, 30,k=1)
	
}
