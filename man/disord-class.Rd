\name{disord-class}
\docType{class}
\alias{disord-class}
\alias{index-class}
\alias{[}
\alias{[.disord}
\alias{[,disord-method}
\alias{[,disord,index,ANY-method}
\alias{[,disord,index,missing-method}
\alias{[,disord,ANY,ANY-method}
\alias{[,disord,index,index-method}
\alias{[,disord,missing,index-method}
\alias{[,disord,disord,missing-method}
\alias{[,disord,disord,missing,ANY-method}
\alias{[,disord,missing,missing-method}
\alias{[,disord,index,ANY,ANY-method}
\alias{[,disord,index,missing,ANY-method}
\alias{[,ANY,disord,ANY-method}
\alias{[<-}
\alias{[<-.disord}
\alias{[<-,disord-method}
\alias{[<-,disord,ANY,ANY-method}
\alias{[<-,disord,index,ANY,ANY-method}
\alias{[<-,disord,index,missing,ANY-method}
\alias{[<-,disord,index,missing,numeric-method}
\alias{[<-,disord,index,missing,disord-method}
\alias{[<-,disord,missing,missing,numeric-method}
\alias{[<-,disord,missing,missing,disord-method}
\alias{[<-,disord,index,index-method}
\alias{[<-,disord,index,missing-method}
\alias{[<-,disord,missing,index-method}
\alias{[<-,disord,missing,missing-method}
\alias{[<-,disord,disord,missing,ANY-method}
\alias{[<-,disord,disord,missing,disord-method}
\alias{[<-,disord,disord,missing-method}
\alias{[<-,disord,missing,missing,ANY-method}
\alias{Arith,ANY,disord-method}
\alias{Arith,disord,ANY-method}
\alias{Arith,disord,disord-method}
\alias{Arith,disord,missing-method}
\title{Class \code{"disord"}}
\description{
  The \code{disord} class provides basic arithmetic and extract/replace
  methods for disord objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("disord", ...)},
although functions \code{disord()} and (eventually) \code{as.disord()}
are more user-friendly.

Class \dfn{index} is taken from the excellent \CRANpkg{Matrix} package
and is a \code{setClassUnion()} of classes \code{numeric},
\code{logical}, and \code{character}.

}
\section{Slots}{
  \describe{
    \item{\code{v}:}{Object of class \code{vector} that specifes the
      elements}
    \item{\code{h}:}{Object of class \code{character} that
      specifies the hash code}
  }
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "disord", i = "ANY", j = "ANY")}: ... }
    \item{[}{\code{signature(x = "disord", i = "index", j = "index")}: ... }
    \item{[}{\code{signature(x = "disord", i = "index", j = "missing")}: ... }
    \item{[}{\code{signature(x = "disord", i = "missing", j = "index")}: ... }
    \item{[}{\code{signature(x = "disord", i = "missing", j = "missing")}: ... }
    \item{[}{\code{signature(x = "disord", i = "matrix", j = "missing")}: ... }
    \item{[<-}{\code{signature(x = "disord", i = "index", j = "index")}: ... }
    \item{[<-}{\code{signature(x = "disord", i = "index", j = "missing")}: ... }
    \item{[<-}{\code{signature(x = "disord", i = "missing", j = "index")}: ... }
    \item{[<-}{\code{signature(x = "disord", i = "matrix", j = "missing")}: ... }
    \item{[<-}{\code{signature(x = "disord", i = "missing", j = "missing")}: ... }
    \item{Arith}{\code{signature(e1 = "ANY", e2 = "disord")}: ... }
    \item{Arith}{\code{signature(e1 = "disord", e2 = "ANY")}: ... }
    \item{Arith}{\code{signature(e1 = "disord", e2 = "disord")}: ... }
    \item{Arith}{\code{signature(e1 = "disord", e2 = "missing")}: ... }
  }

  The extraction method takes a \code{drop} argument which if
  \code{TRUE}, returns the \code{drop()} of its value.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{drop}}}
\examples{showClass("disord")}
\keyword{classes}
