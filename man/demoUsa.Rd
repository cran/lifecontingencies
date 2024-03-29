% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{demoUsa}
\alias{demoUsa}
\title{United States Social Security life tables}
\format{
A \code{data.frame} containing people surviving at the beginning of "age" at 2007, 2000, and 1990
split by gender
}
\source{
See \url{https://www.ssa.gov/oact/NOTES/as120/LifeTables_Body.html}
}
\usage{
demoUsa
}
\description{
This data set contains period life tables for years 1990, 2000 and 2007. Both males and 
females life tables are reported.
}
\details{
Reported age is truncated at the last age with lx>0.
}
\examples{
data(demoUsa)
head(demoUsa)
}
\keyword{datasets}
