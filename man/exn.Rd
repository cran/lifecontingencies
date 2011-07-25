\name{exn}
\alias{exn}

\title{
Function/method to calculate the expected life.
}
\description{
This method calculates the expected life span between x and x+n.
}
\usage{
exn(object, x, n)
}

\arguments{
  \item{object}{
A lifetable object.
}
  \item{x}{
Age from which the life span should be calculated.
}
  \item{n}{
Time until which the expected life should be calculated. Assumed omega - x whether missing.
}
}
\value{
A numeric value representing the expected life span.
}
\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato.
}

\section{Warning }{
	The function is provided as is, without any warranty regarding the accuracy of calculations. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}

\seealso{
\code{\linkS4class{lifetable}}
}

\examples{
\dontrun{

	#loads and show
	data(demoita)
	head(demoita)
	#creates ips55 life and actuarial table
	lxIPS55M<-with(demoita, IPS55M)
	pos2Remove<-which(lxIPS55M %in% c(0,NA)) #remove missing and zeros
	lxIPS55M<-lxIPS55M[-pos2Remove]
	xIPS55M<-seq(0,length(lxIPS55M)-1,1)
	#create the table
	IPS55Mlt=new("lifetable",x=xIPS55M,lx=lxIPS55M,name="IPS55M")
	#shows number of death between 90 and 92
	dxt(object=IPS55Mlt, x=90, t=2)
}
}
