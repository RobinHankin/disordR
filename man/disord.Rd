\name{disord}
\alias{disord}
\alias{hash}
\alias{hashcal}
\alias{as_disord}
\alias{is.disord}
\alias{accessors}
\alias{disord}
\alias{elements}
\alias{disord<-}
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
hashcal(x,ultra_strict=FALSE)
disord(v,h,drop=TRUE)
elements(x)
}
\arguments{
  \item{x}{Object of class \code{disord}}
  \item{v}{Vector of coefficients}
  \item{h}{Hash code}
  \item{drop}{Boolean, with default \code{FALSE} meaning to return a
    disord object and \code{TRUE} meaning to call \code{drop()} before
    returning}
  \item{ultra_strict}{Boolean, with default \code{FALSE} meaning to use
    just \code{x} to generate the hash, and \code{TRUE} meaning to use
    the date and a random number as well [this ensures that the hash is
    generated only once]}
}
\details{

  A detailed vignette is provided that motivates the package.  In
  applications such as the \CRANpkg{mvp} or \CRANpkg{clifford} packages,
  the user will not need to even think about the \CRANpkg{disordR}
  package: it works in the background.  The purpose of the package is to
  trap plausible idiom that is ill-defined (implementation-specific) and
  return an informative error, rather than returning a possibly
  incorrect result.

  The package provides a single \proglang{S4} class, \code{disord},
  which has two slots, \code{.Data} and \code{hash}.

  Function \code{disord()} takes an R object such as a vector or list
  and returns a \code{disord} object, which is useful in the context of
  the \code{STL} map class.

  Function \code{hash()} returns the hash of an object (compare
  \code{hashcal()} which is used to actually calculate the hash code).

  The package detects acceptable and forbidden operations using hash
  codes: function \code{consistent()} checks for its arguments having
  the same hash code, and thus their elements can be paired up
  (e.g. added).  Idiomatically, \code{a \%~\% b} is equivalent to
  \code{consistent(a,b)}.

  Function \code{elements()} takes a \code{disord} and returns a regular
  \R object, typically a vector or a list.
   
}
\value{Boolean, hash code, or object of class \code{disord} as
  appropriate.}
\author{Robin K. S. Hankin}
\examples{

(a <- rdis())
(b <- rdis())

a + 2*a + 2^a  # fine
# a + b # this would give an error if executed

a[a<0.5] <- 0       # round down; replacement works as expected

elements(a)

}
\keyword{symbolmath}
