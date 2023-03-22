\name{disindex-class}
\docType{class}
\alias{disindex-class}
\title{Experimental class \code{"disindex"}}
\description{

  Experimental \code{disindex} class provides a \code{disordR}-compliant
  method for indexing \code{disord} objects via the \code{which()}.

}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("disord", ...)},
although functions \code{disord()} and (eventually) \code{as.disord()}
are more user-friendly.

}
\section{Slots}{
  \describe{
    \item{\code{value}:}{Numeric vector}
    \item{\code{hash}:}{Object of class \code{character} that
      specifies the hash code}
  }
}

\author{Robin K. S. Hankin}
\examples{showClass("disord")}
\keyword{classes}
