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
  If its argument is disord object all of whose elements are
  identical, one very frequently wants to drop the disord attribute and
  coerce to a vector.   The criterion of all identical elements is given
  by \code{allsame()}.
}
\author{Robin K. S. Hankin}
\examples{
drop(disord(c(3,3,3,3,3,3,2,3,3,3,3,3)))
drop(disord(c(3,3,3,3,3,3,3,3,3,3,3,3)))
}
