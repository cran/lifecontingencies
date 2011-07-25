\name{Axn}
\alias{Axn}

\title{
	Function to evaluate life insurance.
}
\description{
	This function evaluates life insurance payable at the moment of deaths.
}
\usage{
	Axn(actuarialtable, x, n, m, type = "EV")
}

\arguments{
  \item{actuarialtable}{
	An actuarial table object.
}
  \item{x}{
	Age of the annuity.
}
  \item{n}{
	Number of termso the annuity, if missing annuity is intended to last until death.
}
  \item{m}{
	Postponement period, even fractional, if missing assumed to be 1.
}
  \item{type}{
	A string, eithed "EV" (default value) or "ST".
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
	Stochastic calculations have not been implemented yet in case of payments at the end of the month of death.
}



\section{Warning }{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
	\code{\link{axn}}, \code{\link{Exn}}
}
\examples{
	\dontrun{
		#assumes SOA example life table to be load
		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		#evaluate the value of a 40 years term life insurance for an aged 25
		Axn(actuarialtable=soa08Act, x=25, n=40) 
	}
}

