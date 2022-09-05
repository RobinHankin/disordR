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
hashcal(x)
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
}
\details{
  The package provides a single \proglang{S4} class, \code{disord}.  A
  detailed vignette is provided that motivates the package.
  
  Function \code{hash()} is the extractor function:

  \code{`hash` <- function(x){x@hash}}

  Compare \code{hashcal()} which is used to actually calculate the hash
  code for an object.  Currently

  \code{`hashcal` <- function(x){digest::sha1(x)}}

  Function \code{disord()} takes a vector and returns a \code{disord}
  object; function \code{elements()} takes a \code{disord} and returns a
  vector.  These are the central function of the package.

  Checking for matching hash codes is done by function
  \code{consistent()}.  This checks that its arguments have the same
  hash code, and thus their elements can be paired up (e.g. added).
  Idiom \code{a \%~\% b} is equivalent to \code{consistent(a,b)}.
   
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
