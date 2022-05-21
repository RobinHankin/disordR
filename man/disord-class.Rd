\name{disord-class}
\docType{class}
\alias{disord-class}
\title{Class \code{"disord"}}
\description{
  The \code{disord} class provides basic arithmetic and extract/replace
  methods for disord objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("disord", ...)},
although functions \code{disord()} and (eventually) \code{as.disord()}
are more user-friendly.

}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{vector} that specifies the
      elements}
    \item{\code{hash}:}{Object of class \code{character} that
      specifies the hash code}
  }
}

\author{Robin K. S. Hankin}
\examples{showClass("disord")}
\keyword{classes}
