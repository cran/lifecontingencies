\name{presentValue}
\alias{presentValue}

\title{
Present value of a series of cash flows.
}
\description{
This function evaluates the present values of a series of cashFlows, due at specific times, using a specific discount curve and a 
vector of probabilities.
}
\usage{
presentValue(cashFlows, timeIds, interestRates, probabilities)
}

\arguments{
  \item{cashFlows}{
Vector of cashFlow, must be coherent with \code{timeIds}
}
  \item{timeIds}{
	Vector of points of time where \code{cashFlows} are due.
}
  \item{interestRates}{
	A numeric value or a time-size vector of interest rate used to discount cahs flow.
}
  \item{probabilities}{
	Optional vector of probabilities.
}
}
\details{
	\code{probabilities} is optional, a sequence of 1 length of timeIds is assumed.
	Interest rate shall be a fixed number or a vector of the same size of \code{timeIds}.
}
\value{
	A numeric value representing the present value of cashFlows vector, or the actuarial value if probabilities are provided.
}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.
}
\author{
Giorgio A. Spedicato
}
\note{
This simple function is the kernel working core of the package and of the whole classical financial and mathematics.
}


\section{Warning }{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}




\seealso{
\code{\link{annuity}}, \code{\link{axn}}
}
\examples{
 #simple example
 cf=c(10,10,10)	#$10 of payments one per year for three years
 t=c(1,2,3) #years
 p=c(1,1,1) #assume payments certainty
 #assume 3% of interest rate
presentValue(cashFlows=cf, timeIds=t, interestRates=0.03, probabilities=p)
}

