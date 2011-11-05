\name{real2Nominal}
\alias{real2Nominal}

\title{
Function to switch from  APR to nominal m-periods interest rate
}
\description{
This functions switches from nominal to effective APR.
}
\usage{
real2Nominal(i, m = 1)
}

\arguments{
  \item{i}{
Annual effective interest rate.
}
  \item{m}{Numbers of compounding period.}
}
\details{
The functtion follows the equation 
\eqn{\left( {1 + i} \right)^t = (1 - d)^{ - t} = 
\left( {1 + \frac{{{i^{\left( m \right)}}}}{m}} \right)^{m*t} = 
\left( 1 - \frac{{{d^{\left( m \right)}}}}{m} \right)^{ - m*t}}

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
	Fractional payments have not been implemented yet.
}

\seealso{
\code{\link{nominal2Real}}
}
\examples{
#an APR of 12\% corresponds to an nominal interest rate compounded monthly of
real2Nominal(i=0.12, m = 12)

}
