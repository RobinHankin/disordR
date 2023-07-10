\name{Compare-methods}
\docType{methods}
\alias{Compare-methods}
\alias{Compare,disord,ANY-method}
\alias{Compare,disord,disord-method}
\alias{Compare,ANY,disord-method}
\alias{any_compare_disord}
\alias{disord_compare_any}
\alias{disord_compare_disord}
\title{Methods for comparison for \code{disord} objects}
\description{
  Methods for comparison (greater than, etc) in package. 
}
\section{Methods}{
  \describe{
    \item{Compare}{\code{signature(e1="disord", e2="disord")}: ... }
    \item{Compare}{\code{signature(e1="disord", e2="ANY"   )}: ... }
    \item{Compare}{\code{signature(e1="ANY"   , e2="disord")}: ... }
  }
}

\note{

All the comparison methods use \code{drop=TRUE} to avoid inconsistent
results when all the values are the same.
  
}
\examples{
rdis() > 4
rdis() > 1000
}
\keyword{methods}
\keyword{math}
