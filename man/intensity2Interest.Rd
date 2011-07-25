\name{intensity2Interest}
\alias{intensity2Interest}

\title{
Function to conver the force of interest into yearly interest rate.
}
\description{
This function applies a financial mathematics identity.
}
\usage{
intensity2Interest(intensity)
}

\arguments{
  \item{intensity}{The value of the intensity force to be converted.}
}

\value{A numeric value}
\references{
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.
}
\author{
Giorgio A. Spedicato
}

\seealso{
\code{\link{annuity}}, \code{\link{interest2Intensity}}
}
\examples{

#a force of intestest of 0.02 corresponds to an APR of 
intensity2Interest(intensity=0.02)

}

