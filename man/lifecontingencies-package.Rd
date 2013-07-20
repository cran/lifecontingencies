\name{lifecontingencies-package}
\alias{lifecontingencies-package}
\alias{lifecontingencies}
\docType{package}
\title{
Package to perform actuarial mathematics on life contingencies and classical financial mathematics calculations.
}
\description{
	The lifecontingencies package performs standard financial, demographic and actuarial mathematics calculation. The main purpose of 
	the package is to provide a comprehensive set of tools to perform risk assessment of life contingent insurances.  
}
\details{
\tabular{ll}{
Package: \tab lifecontingencies\cr
Type: \tab Package\cr
Version: \tab 0.9.8.6\cr
Date: \tab 2013-07-20\cr
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
Work in progress.
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


##financial mathematics example

#calculates monthly installment of a loan of 100,000, 
#interest rate 0.05

i=0.05
monthlyInt=(1+i)^(1/12)-1
Capital=100000
#Montly installment

R=1/12*Capital/annuity(i=i, n=10,k=12, type = "immediate")
R
balance=numeric(10*12+1)
capitals=numeric(10*12+1)
interests=numeric(10*12+1)
balance[1]=Capital
interests[1]=0
capitals[1]=0

for(i in (2:121))	{
			balance[i]=balance[i-1]*(1+monthlyInt)-R
			interests[i]=balance[i-1]*monthlyInt
			capitals[i]=R-interests[i]
			}
loanSummary=data.frame(rate=c(0, rep(R,10*12)), 
	balance, interests, capitals)

head(loanSummary)

tail(loanSummary)

##actuarial mathematics example

#APV of an annuity

		data(soaLt)
		soa08Act=with(soaLt, new("actuarialtable",interest=0.06,
		x=x,lx=Ix,name="SOA2008"))
		#evaluate and life-long annuity for an aged 65
		axn(soa08Act, x=65) 

}
