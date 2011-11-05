\name{nominal2Real}
\alias{nominal2Real}


\title{
Function to calculate the nominal interest rate.
}
\description{This function evaluates the nominal m-periods interest rate (or the discount rate) given the effective yearly interest rate.}
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
nominal2Real(i=0.12, m = 12, "interest")

}

