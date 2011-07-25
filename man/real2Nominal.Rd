\name{real2Nominal}
\alias{real2Nominal}

\title{
Function to switch from effective APR to nominal k-periods APR
}
\description{
This functions switchs from nominal to effective APR.
}
\usage{
real2Nominal(interestRate, periods = 1)
}

\arguments{
  \item{interestRate}{
Annual effective interest rate.
}
  \item{periods}{
Numbers of subperiods.
}
}
\details{
	Fractional payments have not been implemented yet.
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
#an effective percentage rate of 12% corresponds to an nominal APR of
real2Nominal(interestRate=0.12, periods = 12)

}
