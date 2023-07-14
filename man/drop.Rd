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
  usually wants to drop the disord attribute and coerce to a vector.
  This can be done without breaking \code{disordR} discipline.  Function
  \code{disord()} takes a \code{drop} argument, defaulting to
  \code{TRUE}, which drops the \code{disord} class from its return value
  if all the elements are the same.

  Similarly, function \code{drop()} takes a disord object and if all
  elements are identical it returns the elements in the form of a
  vector.  Some extraction methods take a \code{drop} argument, which
  does the same thing if \code{TRUE}.  This is only useful for disord
  objects created with \code{disord(...,drop=FALSE)}
  
  The \code{drop} functionality is conceptually similar to the
  \code{drop} argument of base \R's array extraction, as in

  \preformatted{
     a <- matrix(1:30,5,6)
     a[1,,drop=TRUE]
     a[1,,drop=FALSE]
   }

  Function \code{allsame()} takes a vector and returns \code{TRUE} if
  all elements are identical.
  
}
\value{
  Function \code{drop()} returns either a vector or object of class
  \code{disord} as appropriate; \code{allsame()} returns a Boolean.
}
\author{Robin K. S. Hankin}
\examples{
disord(c(3,3,3,3,3))             # default is drop=TRUE
disord(c(3,3,3,3,3),drop=FALSE)  # retains disord class

drop(disord(c(3,3,3,3),drop=FALSE)) 

## In extraction, argument drop discards disorderliness when possible:
a <- rdis()
a
a[] <- 6 # a becomes a vector
a

}
