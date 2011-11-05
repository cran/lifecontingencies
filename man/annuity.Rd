\name{annuity}
\alias{annuity}

\title{
Annuity function
}
\description{
Function to calculate present value of annuities-certain.
}
\usage{
annuity(i, n,m=1,type = "immediate")
}

\arguments{
  \item{i}{
	Effective interest rate expressed in decimal form. E.g. 0.03 means 3\%. It can be a vector of interest rates of the same length
	of periods. If i = \code{infinity} then it returns the value of a perpetuity.
}
  \item{n}{
	Number of payments.
}
  \item{m}{
	Frequency of payments. A payment of \eqn{m^-1} is supposed to be performed at the end of each due 
	date.
}
  \item{type}{
	A string, either "immediate" or "due".
}
}
\details{
	This function calculates the present value of a stream of fixed payments separated by equal interval of time. Annuity immediate has the 
	fist payment at time t=0, while an annuity due has the first payment at time t=1.
}
\value{
	A string, either "immediate" or "due".
}
\references{
	Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
	Giorgio A. Spedicato
}
\note{
	The value returned by annuity function derives from direct calculation of the discounted cash flow and 
	not from formulas, like \eqn{{a^{\left( m \right)}}_{\left. {\overline {\, 
 n \,}}\! \right| } = \frac{{1 - {v^n}}}{{{i^{\left( m \right)}}}}}.
}



\seealso{
\code{\link{accumulatedValue}}
}
\examples{
	# The present value of 5 payments of 1000 at one year interval beginnin now when the interest rate is 2.5% is
	1000*annuity(i=0.05, n=5, type = "due")
}

