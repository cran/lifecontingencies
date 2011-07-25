\name{dxt}
\alias{dxt}

\title{
Deaths function.
}
\description{Function to calculate deaths between time intervals
}
\usage{
dxt(object, x, t)
}

\arguments{
  \item{object}{
A lifetable or actuarialtable object.
}
  \item{x}{
Age since with the death shall be evaluated.
}
  \item{t}{
Time span, assumed one if missing.
}
}
\details{
This simple function is essential for more useful actuarial functions.
}
\value{
An integer value
}
\references{	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.

}
\author{
Giorgio A. Spedicato
}

\section{Warning }{
	The function is provided as is, without any guarantee regarding the accuracy of calculation. The author disclaims any liability for eventual 
	losses arising from direct or indirect use of this software.
}


\seealso{
\code{\link{pxt}}
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
	#deaths between 90 and 92
	dxt(object=IPS55Mlt, x=90, t=2)
}
}

