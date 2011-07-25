\name{decreasingAnnuity}
\alias{decreasingAnnuity}

\title{
	Function to evaluate a decreasing annuity.
}
\description{
	This function evalutates decreasing annuities.
}
\usage{
decreasingAnnuity(interestRate, periods)
}

\arguments{
  \item{interestRate}{
	A numeric value representing the interest rate.
}
  \item{periods}{
	The number of periods.
}
}
\details{
	A decreasing annuity has the following flows of payments: n, n-1, n-2,...
}
\value{
	A numeric value reporting the present value of the decreasing cash flows.
}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
	Giorgio A. Spedicato
}
\note{
	Fractional payments have not implemented yet.
}

\section{Warning }{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
\code{\link{annuity}},\code{\link{increasingAnnuity}},\code{\link{DAxn}}
}
\examples{
	#the present value of 10, 9, 8,....,0 payable at the end of the period
	#for 10 periods is
	decreasingAnnuity(interestRate=0.03, periods=10)
	#assuming a 3% interest rate
}

