\name{nominal2Real}
\alias{nominal2Real}


\title{
Function to convert nominal interest rate to effective interest rate.
}
\description{This function evaluates the real (also known as effective) interest (or discount) rate 
 given the nominal interest (or discount) rate compounded m-times.}
\usage{nominal2Real(i, k = 1, type="interest")
}

\arguments{
  \item{i}{Nominal interest rate.}
  \item{k}{Frequency of compounding}
  \item{type}{Either "interest" or "discount" rate}
}
\value{A numeric value is returned.}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.
}
\author{Giorgio A. Spedicato}
\note{The accumulation function is assumed to follow compound interest law, i.e.,
	\eqn{A\left( t \right) = \left( 1 + \frac{i^k}{k} \right)^{kt} }
	}
\seealso{\code{\link{real2Nominal}}}
\examples{#a nominal rate of 0.12 equates an APR of
nominal2Real(i=0.12, k = 12, "interest")}

