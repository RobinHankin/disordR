\name{disord}
\alias{disord}
\alias{hash}
\alias{as_disord}
\alias{is.disord}
\alias{accessors}
\alias{consistent}
\alias{disord}
\alias{disord<-}
\alias{\%~\%}
\title{Functionality for \code{disord} objects}
\description{
  Allows arithmetic operators to be used for disord objects; the
  canonical application is coefficients of multivariate polynomials (as
  in the \CRANpkg{mvp} package).  The issue is that the storage order of
  disord objects is implementation-specific but the order (whatever it
  is) must be consistent between the list of keys and values in an
  associative array.
}
\usage{
is.disord(x)
hash(x)
disord(v,h)
consistent(x,y)
x \%~\% y
}
\arguments{
  \item{x,y}{Objects of class \code{disord}}
\item{v}{Vector of coefficients}
\item{h}{Hash code}
}
\details{
  The package provides a single \proglang{S4} class, \code{disord}.  A
  detailed vignette is provided that motivates the package.
}
\value{Boolean, hash code, or object of class \code{disord} as
  appropriate.}
\author{Robin K. S. Hankin}
\examples{

a <- rdis()
b <- rdis()

a + 2*a + 2^a  # fine
# a + b # error

a[a<0.5] <- 0       # round small entries down


}
\keyword{symbolmath}
