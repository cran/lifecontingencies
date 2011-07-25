\name{nominal2Real}
\alias{nominal2Real}


\title{
Function to calculate the nominal interest rate.
}
\description{This function evaluates the nominal k-periods interest rate from the effective yearly interest rate.}
\usage{nominal2Real(interestRate, periods = 1)
}

\arguments{
  \item{interestRate}{Annual effective interest rate.}
  \item{periods}{Numbers of subperiods.}
}
\value{A numeric value is returned.}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.
}

\author{
	Giorgio A. Spedicato
}
\note{
	Fractional payments have not been implemented yet.
}

\seealso{
\code{\link{real2Nominal}}
}
\examples{
#a nominal percentage rate of 12% corresponds to an effective APR of
nominal2Real(interestRate=0.12, periods = 12)

}

