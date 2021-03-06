% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{demoJapan}
\alias{demoJapan}
\title{Japan Mortality Rates for life table construction}
\format{
A data frame with 110 observations on the following 3 variables.
\describe{ \item{\code{JP8587M}}{Male life table} \item{\code{JP8587F}}{Female life table} \item{\code{age}}{Attained age} }
}
\source{
Society of Actuaries mortality web site
}
\usage{
data(demoJapan)
}
\description{
Two yearly mortality rates for each age
}
\details{
See the references link for details.
}
\examples{
data(demoJapan)
head(demoJapan)
}
\references{
\url{https://mort.soa.org/}
}
\keyword{datasets}
