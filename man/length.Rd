\name{length}
\alias{length}
\alias{length.disord}
\alias{length<-}
\alias{length<-.disord}
\alias{length,disord-method}
\alias{length<-,disord-method}
\title{Length of a disord object}
\description{Get or set the length of \code{disord} objects}
\usage{\S4method{length}{disord}(x)}
\arguments{\item{x}{A \code{disord} object}}
\value{Integer}
\details{Returns the number of elements of \code{x}.  It is not possible
  to set the length of a \code{disord} object.}
\author{Robin K. S. Hankin}
\examples{
length(disord(1:100))
}
