\name{increasingAnnuity}
\alias{increasingAnnuity}

\title{
	Function to evaluate an increasing annuity.
}
\description{
	This function evalutates non - stochastic increasing annuities.
}
\usage{
	increasingAnnuity(i, n, type = "immediate")
}

\arguments{
  \item{i}{
	A numeric value representing the interest rate.
}
  \item{n}{
	The number of periods.
}
  \item{type}{
	Either "immediate" or "due"
}
}
\details{
	An increasing annuity has the following flows of payments: 1,2,...,n-1,n
}
\value{
	The value of the annuity.
}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
	Giorgio A. Spedicato
}
\note{
	Fractional annuities have not been implemented yet.
}

\section{Warning }{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. We disclaim any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
\code{\link{decreasingAnnuity}},\code{\link{IAxn}}
}
\examples{
	#the present value of 1,2,...,n-1,n payable at the end of the period
	#for 10 periods is
	increasingAnnuity(i=0.03, n=10)
	#assuming a 3% interest rate
}

