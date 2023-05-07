\name{disindex-class}
\docType{class}
\alias{disindex-class}
\alias{disindex}
\alias{values}
\title{Experimental class \code{"disindex"}}
\description{

  Experimental \code{disindex} class provides a \code{disordR}-compliant
  method for indexing \code{disord} objects.  The idea is that
  \code{which(x)}, where \code{x} is Boolean of class \code{disord},
  should have meaning under \code{disordR} discipline.  Thus
  \code{which()} gives a \code{disindex} object.  This object can be
  used as an index for other \code{disord} objects.  One application
  would be the \code{dismat} class of matrices, currently under
  development.

  Function \code{values()} coerces its argument to an integer vector.

}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("disindex", ...)},
although \code{which()} is more natural.

}
\section{Slots}{
  \describe{
    \item{\code{value}:}{Numeric vector}
    \item{\code{hash}:}{Object of class \code{character} that
      specifies the hash code}
  }
}

\author{Robin K. S. Hankin}
\examples{


(x <- disord(c(1,2,1,2,2,7)))

x==2
w <- which(x==2)
w

x[w] <- 100
x





}
\keyword{classes}
