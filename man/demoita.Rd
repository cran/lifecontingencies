\name{demoIta}
\alias{demoIta}
\docType{data}
\title{
Sample of Italian population life tables for males and females
}
\description{
This dataset reports four pairs of life table, regarding Italian population. These table can be used to create 
life table objects and actuarial tables object.
}
\usage{data(demoIta)}
\format{
  A data frame with 121 observations on the following 9 variables.
  \describe{
    \item{\code{X}}{a numeric vector, representing ages from 0 to omega.}
    \item{\code{SIM92}}{a numeric vector, 1992 cross section general population females life table }
    \item{\code{SIF92}}{a numeric vector, 1992 cross section general population males life table}
    \item{\code{SIM00}}{a numeric vector, 2000 cross section general population females life table }
    \item{\code{SIF00}}{a numeric vector, 2000 cross section general population males life table}
    \item{\code{SIM02}}{a numeric vector, 2002 cross section general population females life table}
    \item{\code{SIF02}}{a numeric vector, 2002 cross section general population males life table}
    \item{\code{RG48M}}{a numeric vector, RG48 projected males life table}
    \item{\code{RG48F}}{a numeric vector, RG48 projected females life table}
    \item{\code{IPS55M}}{a numeric vector, IPS55 projected males life table}
    \item{\code{IPS55F}}{a numeric vector, IPS55 projected females life table}
  }
}
\details{
	These table contains the vectors of survival at the beginning of life years and are the building block of both 
	lifetable and actuarialtable classes.
}
\source{
	These tables comes from Italian national statistical bureau (ISTAT) for SI series, government Ministry of Economics (Ragioneria Generale dello Stato) for
	RG48 or from Insurers' industrial association IPS55. RG48 represents the projected survival table for the 1948 born cohort, while IPS55 represents 
	the projected survival table for the 1955 born cohort.
}
\references{
	\url{http://www.ania.it/private/documents/comunicazioni2005/PROT0252COMU.pdf}
	\url{http://demo.istat.it/unitav/index.html?lingua=ita}
}
\examples{
	#loads and show
	data(demoIta)
	head(demoIta)
	#creates sim92 life and actuarial table
	lxsim92<-demoIta$SIM92

	lxsim92<-lxsim92[!is.na(lxsim92) & lxsim92!=0]
	xsim92<-seq(0,length(lxsim92)-1,1)
	#create the table
	sim92lt=new("lifetable",x=xsim92,lx=lxsim92,name="SIM92")
	plot(sim92lt)
}
\keyword{datasets}
