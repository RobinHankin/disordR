\name{extract}
\alias{extract}
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
\alias{[,disord,missing,missing,ANY-method}
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
\title{Extraction and replacement methods for class \code{"disord"}}
\description{
  The \code{disord} class provides basic arithmetic and extract/replace
  methods for disord objects.

Class \dfn{index} is taken from the excellent \CRANpkg{Matrix} package
and is a \code{setClassUnion()} of classes \code{numeric},
\code{logical}, and \code{character}.

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
  \code{TRUE}, returns the \code{drop()} of its value.  Extraction, as
  in \code{x[i]}, is rarely useful.  It is only defined if one extracts
  either all, or none, of the elements: anything else is undefined.
  Note that the hash code is unchanged if all elements are extracted
  (because the order might have changed) but unchanged if none are
  (because there is only one way to extract no elements).
  
}
\author{Robin K. S. Hankin}
\note{

  Package versions prior to \code{disordR_0.0-9-6} allowed idiom such as

  \preformatted{
    a <- disord(1:9)
    a[a<3] + a[a>7]
  }

  but this is now disallowed.  The issue is discussed in
  \code{inst/note_on_extraction.Rmd}.
}

  
\seealso{\code{\link{drop}}}
\examples{
a <- disord(sample(9))
a
a + 6*a^2
a[a>5]  # "give me all elements of a that exceed 5"

a[] # a disord object, same elements as 'a', but with a different hash

a[a<5] <- a[a<5] + 100  # "replace all elements of 'a' less than 5 with their value plus 100"
a

## Following expressions would return an error if executed:
if(FALSE){
  a[1]
  a[1] <- 44
  a[1:2] <- a[3:4]
}

b <- disord(sample(9))
## Following expressions would also return an error if executed:
if(FALSE){
  a+b  # (not really an example of extraction)
  a[b>5]
  a[b>5] <- 100
  a[b>5] <- a[b>5] + 44
}

}
