\name{drop}
\alias{drop}
\alias{drop,disord-method}
\alias{allsame}
\title{Drop redundant information}
\description{Coerce \code{disord} objects to vector when this makes sense}
\usage{
drop(x)
allsame(x)
}
\arguments{\item{x}{\code{disord} object}}
\details{
  If one has a disord object all of whose elements are identical, one
  very frequently wants to drop the disord attribute and coerce to a
  vector.  This can be done without ambiguity.  Function \code{drop()}
  takes a disord object, and if all elements are identical returns the
  elements in the form of a vector.  Some extraction methods take a
  \code{drop} argument, which does the same thing if \code{TRUE}.

  The \code{drop} functionality is conceptually similar to the
  \code{drop} argument of base R's array extraction, as in

  \preformatted{
  a <- matrix(1:30,5,6)
  a[1,,drop=TRUE]
  a[1,,drop=FALSE]
  }	
  


}
\value{Function \code{drop()} returns either a vector or object of class
  \code{disord} as appropriate; \code{allsame()} returns a Boolean.}
\author{Robin K. S. Hankin}
\examples{
drop(disord(c(3,3,3,3,3,3,2,3,3,3,3,3)))
drop(disord(c(3,3,3,3,3,3,3,3,3,3,3,3)))

## In extraction, argument drop discards disorderliness when possible:
a <- rdis()
a
a[] <- 6             # a becomes a vector
b <- rdis()
b[drop=FALSE] <- 6   # b stays a disord [NB this behaviour might change in the future]
}
