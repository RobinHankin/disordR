\name{Compare-methods}
\docType{methods}
\alias{Compare-methods}
\alias{Compare,disord,ANY-method}
\alias{Compare,disord,disord-method}
\alias{Compare,ANY,disord-method}
\alias{any_compare_disord}
\alias{disord_compare_any}
\alias{disord_compare_disord}
\title{Methods for comparison of \code{disord} objects}
\description{
Arithmetic comparison methods (greater than, etc) for \code{disord}
objects.
}
\section{Methods}{
  \describe{
    \item{Compare}{\code{signature(e1="disord", e2="disord")}:
      Dispatched to \code{disord_compare_disord()}}
    \item{Compare}{\code{signature(e1="disord", e2="ANY")}:
      Dispatched to \code{disord_compare_any()}}
    \item{Compare}{\code{signature(e1="ANY", e2="disord")}:
      Dispatched to \code{any_compare_disord()}}
  }
}
\note{

All the comparison methods use \code{drop=TRUE} to avoid inconsistent
results when all the values are the same [that is, all \code{TRUE} or
all \code{FALSE}].  Comparing two \code{disord} objects requires their
hash code to agree as per \pkg{disordR} discipline.  Comparing a
\code{disord} with a numeric returns a \code{disord} object.  In each
case, the hash code of the original object is preserved in the returned
value.
  
}
\examples{
rdis() > 4
rdis() > 1000
}
\keyword{methods}
\keyword{math}
