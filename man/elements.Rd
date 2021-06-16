\name{elements}
\alias{elements}
\title{Extract elements of a disord object}
\description{
  A disord object contains elements in an arbitrary order.  Function
  \code{elements()} shows these elements in the form of an ordinary
  vector. 
}
\usage{elements(x)}
\arguments{\item{x}{Object of class \code{disord}}}
\value{Returns a vector}
\author{Robin K. S. Hankin}
\note{
This function imposes an arbitrary order on the elements.  The value
returned is thus implementation-specific.
}
\examples{elements(rdis())}