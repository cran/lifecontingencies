\name{qxt}
\alias{qxt}

\title{
Function to return death probability.
}
\description{
This function returns death probability between age x and x+t.
}
\usage{
qxt(object, x, t)
}

\arguments{
  \item{object}{
A lifetable object.
}
  \item{x}{
Age from which survival probability should be calculated.
}
  \item{t}{
Time horizon.
}
}
\details{
It is calculated applying \eqn{1-p_{x}^{t}}
}
\value{
A numeric value representing death probability}

\references{
	Actuarial Mathematics (Second Edition), 1997, by Bowers, N.L., Gerber, H.U., Hickman, J.C., 
	Jones, D.A. and Nesbitt, C.J.
}
\author{
Giorgio A. Spedicato.
}
\note{
This is an S4 method. A function would have been written instead.
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
	#shows survival probability between 90 and 92
	qxt(object=IPS55Mlt, x=90, t=2)
}
}
