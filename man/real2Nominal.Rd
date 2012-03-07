\name{real2Nominal}
\alias{real2Nominal}

\title{
Function to switch from APR to nominal m-periods interest rate
}
\description{
This functions switches from nominal to effective APR.
}
\usage{
real2Nominal(i, k = 1,type="interest")
}

\arguments{
  \item{i}{Annual effective interest rate.}
  \item{k}{Compounding periods.}
  \item{type}{Either "interest" rate of "discount" rate}
}
\details{
The function follows the equation 
\eqn{\left( {1 + i} \right)^t = (1 - d)^{ - t} = 
\left( {1 + \frac{{{i^{\left( k \right)}}}}{k}} \right)^{k*t} = 
\left( 1 - \frac{{{d^{\left( k \right)}}}}{k} \right)^{ - k*t}}
}
\value{
A numeric value
}

\references{
	Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
	Giorgio A. Spedicato
}
\note{
The accumulation function is assumed to follow compound interest law, i.e.,
	\eqn{A\left( t \right) = \left( 1 + \frac{i^k}{k} \right)^{k*t}}
	}
\section{Warning }{
	The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{\code{\link{nominal2Real}}}
\examples{#an APR of 0.12 corresponds to a nominal rate monthly compounded of
 
real2Nominal(i=0.12, k = 12)}
