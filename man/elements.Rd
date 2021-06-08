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
This function should not be used as it imposes an arbitrary order on the
elements, which may differ from implementation to implementation.}
\examples{elements(disord(sample(39)))}