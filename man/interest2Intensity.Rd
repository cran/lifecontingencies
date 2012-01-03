\name{interest2Intensity}
\alias{interest2Intensity}

\title{
Function to obtain the force of interest from APR
}
\description{
This function calculates the force of interest from an annual effective interest rate.
}
\usage{
interest2Intensity(i)
}

\arguments{
  \item{i}{
	A numeric value representing the APR
}
}
\details{
This function has been written for convenience and future usage (e.g. implementing continuous annuities).
}
\value{
A numeric value representing the calculated force of interest.
}
\references{
	Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 
	2008, ACTEX Publications.
}
\author{
Giorgio A. Spedicato
}

\seealso{
\code{\link{intensity2Interest}}
}


\section{Warning }{
	This function is provided as is, without any guarantee regarding the accuracy of calculations. The author disclaims any liability arising by  
	any eventual losses arising from direct or indirect use of this package.
}


\examples{
#an interest rate equal to 0.02 corresponds to a force of interest of of 
interest2Intensity(i=0.02)
}

