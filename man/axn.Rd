\name{axn}
\alias{axn}

\title{
Annuity immediate and due function.
}
\description{
	This function calculates actuarial value of annuities, given an actuarial table. Fractional and deferred annuities can be 
	evaluated.	Moreover it can be used to simulate the stochastic distribution of the annuity value.
}
\usage{
	axn(actuarialtable, x, n, m, k = 1, type = "EV")
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
  \item{k}{
	Number of fractional payments per period, if missing assumed to be 1.
}
  \item{type}{
	A string, eithed "EV" (default value) or "ST" (stocastic realization).
}
}
\details{
	When "ST" has been selected a stochastic value representing a number drawn from the domain of \deqn{
	a_{x}^{n}} is drawn. "EV" calculates the standard stochastic value.
}
\value{
	A numeric value.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
}
\author{
	Giorgio A. Spedicato
}
\note{
	We disclaim any financial responsability arising from the use of this package.
}

\seealso{
	\code{\link{annuity}}, \code{\link{Exn}}
}
\examples{
%	\dontrun{
		#assumes SOA example life table to be load
		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		#evaluate and life-long annuity for an aged 65
		axn(soa08Act, x=65) 
%	}
}

