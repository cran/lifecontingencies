\name{soa08}
\alias{soa08}
\docType{data}
\title{
Society of Actuaries life table actuarial object.
}
\description{
This table has been used by the classical book Actuarial Mathematics and by the Society of Actuaries for US professional examinations.
}
\usage{data(soa08)}
\format{
  The format is:
Formal class 'lifetable' [package ".GlobalEnv"] with 3 slots
  ..@ x   : int [1:95] 0 5 10 15 20 21 22 23 24 25 ...
  ..@ lx  : int [1:95] 10000000 9749503 9705588 9663731 9617802 9607896 9597695 9587169 9576288 9565017 ...
  ..@ name: chr "SOA2008"
}
\details{
Early ages are miss.
}
\source{
\url{http://www.soa.org/files/pdf/edu-2008-spring-mlc-tables.pdf}
}
\references{
Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
}
\examples{
data(soa08)
## maybe str(soa08) ; plot(soa08) ...
}
\keyword{datasets}
