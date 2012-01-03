\name{nominal2Real}
\alias{nominal2Real}


\title{
Function to convert nominal interest rate to real  interest rate.
}
\description{This function evaluates the real (also known as effective) interest (or discount) rate 
 given the nominal interest (or discount) rate compounded m-times.}
\usage{nominal2Real(i, m = 1, type="interest")
}

\arguments{
  \item{i}{Nominal interest rate.}
  \item{m}{Numbers of compounding periods.}
  \item{type}{Either interest rate or discount rate}
}
\value{A numeric value is returned.}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.
}
\author{Giorgio A. Spedicato}
\note{The accumulation function is assumed to follow compound interest law, i.e.,
	\eqn{A\left( t \right) = \left( 1 + \frac{i^m}{m} \right)^{mt} }
	}
\seealso{\code{\link{real2Nominal}}}
\examples{#a nominal rate of 0.12 equates an APR of
nominal2Real(i=0.12, m = 12, "interest")}

