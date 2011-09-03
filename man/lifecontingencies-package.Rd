\name{lifecontingencies-package}
\alias{lifecontingencies-package}
\alias{lifecontingencies}
\docType{package}
\title{
Calculates life contingencies and financial mathematics functions.
}
\description{
	The lifecontingencies package has been developed to aid actuaries to perform standard life contingencies analysis as long as 
	standard financial mathematics calculation.
	Currently most important life contingencies actuarial function have been implemented. Multiple life contingencies misses.
}
\details{
\tabular{ll}{
Package: \tab lifecontingencies\cr
Type: \tab Package\cr
Version: \tab 0.0.2\cr
Date: \tab 2011-08-15\cr
License: \tab GPL-2.0\cr
LazyLoad: \tab yes\cr
}

}
\author{
Giorgio A. Spedicato
Maintainer: <lifecontingencies@statisticaladvisor.com>
}
\references{
Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., Jones, D.A. and Nesbitt, C.J.
Broverman, S.A., Mathematics of Investment and Credit (Fourth Edition), 2008, ACTEX Publications.
}

\note{
	Moreover as an R packages user I'm aware that the 
	current documentation is somewhat inadequate. Nevertheless the current version has been submitted in a hurry in order to provide CRAN the first
	package to evaluate life contingencies.\\
}

\seealso{
	\code{\link{accumulatedValue}}, \code{\link{annuity}}
}

\section{Warning }{
	This package and functions herein are provided as is, without any guarantee regarding the 
	accuracy of calculations. The author disclaims any liability arising by  
	eventual losses due to direct or indirect use of this package. 
}

\examples{

##calculates monthly installment of a loan of $100,000, interest rate 5.00%

intEff=(1+0.05)^(1/12)-1
Capital=100000
#Montly installment

R=Capital/annuity(interestRates=intEff, periods=10*12, type = "immediate")
R
balance=numeric(10*12+1)
capitals=numeric(10*12+1)
interests=numeric(10*12+1)
balance[1]=Capital
interests[1]=0
capitals[1]=0

for(i in (2:121))	{
			balance[i]=balance[i-1]*(1+intEff)-R
			interests[i]=balance[i-1]*intEff
			capitals[i]=R-interests[i]
			}
loanSummary=data.frame(rate=c(0, rep(R,10*12)), balance, interests, capitals)

		#assumes SOA example life table to be load

		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		#evaluate and life-long annuity for an aged 65
		axn(soa08Act, x=65) 

}
