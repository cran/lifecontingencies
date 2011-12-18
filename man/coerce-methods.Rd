\name{coerce-methods}
\docType{methods}
\alias{coerce-methods}
\alias{coerce,actuarialtable,data.frame-method}
\alias{coerce,lifetable,data.frame-method}
\alias{coerce,data.frame,lifetable-method}
\title{Methods for Function \code{coerce}}
\description{
 Function \code{coerce}  transforms \code{lifetable} or \code{actuarialtable} objects in \code{data.frame} objects.
}
\section{Methods}{
\describe{

\item{\code{signature(from = "actuarialtable", to = "data.frame")}}{
\code{coerce} method applied to an actuarialtable object to obtain a \code{data.frame} object.
}

\item{\code{signature(from = "lifetable", to = "data.frame")}}{
\code{coerce} method applied to an actuarialtable object to obtain a \code{data.frame} object.
}

\item{\code{signature(from = "data.frame", to = "lifetable")}}{
\code{coerce} method applied to a \code{data.frame} object to obtain a \code{lifetable} object. Requires 
columns "lx" and "x" present.
}
}}
\keyword{methods}

