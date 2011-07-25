\name{interest2Intensity}
\alias{interest2Intensity}

\title{
Function to obtain the force of interest from APR
}
\description{
This function calculates the force of interest from an annual effective interest rate.
}
\usage{
interest2Intensity(interest)
}

\arguments{
  \item{interest}{
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
	This package is provided as is, without any guarantee regarding the accuracy of calculations. The author disclaims any liability arising by  
	any eventual losses arising from direct or indirect use of this package.
}


\examples{


#an intestest of 0.02 corresponds to a force of interest of of 
interest2Intensity(interest=0.02)


}

