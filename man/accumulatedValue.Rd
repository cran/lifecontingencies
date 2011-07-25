\name{accumulatedValue}
\alias{accumulatedValue}

\title{
	Function to evalutate the deterministic accumulated value.
}
\description{
	Financial mathematic function to evaluate a series of payments of 1
}
\usage{
	accumulatedValue(interestRates, periods, type = "immediate")
}

\arguments{
  \item{interestRates}{
	Effective interest rate expressed in decimal form. E.g. 0.03 means 3\%.
}
  \item{periods}{
	Number of terms of payment.
}
  \item{type}{
	A string, either "immediate" or "due".
}
}
\details{
	At the moment no fractional payments have been influenced.
}
\value{
	A numeric value representing the calculated cumulated value.
}
\references{
	Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
	Giorgio A. Spedicato
}
\note{
	Currently fractional payments have not been implemented yet.
}


\section{Warning }{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
\code{\link{annuity}}
}
\examples{
##$100 dollars saved per month after 10 years are...
##assume 2.00 percent effective yearly rate
##

ieff=(1+0.02)^(1/12)-1

100*accumulatedValue(interestRates=ieff, periods=12*10, type="immediate")

}


