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
  in the \pkg{mvp} package).  The issue is that the storage order of
  disord objects is implementation-specific.
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

  Accessing elements of an \code{mvp} object is problematic because the
  order of the terms of an \code{mvp} object is not well-defined.  This
  is because the \code{map} class of the \code{STL} does not specify an
  order for the key-value pairs (and indeed the actual order in which
  they are stored may be implementation dependent).  The situation is
  similar to the \code{hyper2} package which uses the \code{STL} in a
  similar way.
  
  A \code{disord} object is a vector of coefficients of a \code{mvp}
  object.  But it is not a conventional vector; in a conventional
  vector, we can identify the first element unambiguously, and the
  second, and so on.  An \code{mvp} is a map from terms to coefficients,
  and a map has no intrinsic ordering: the maps

  \preformatted{{x -> 1, y -> 3, xy^3 -> 4}}

  and \preformatted{{xy^3 -> 4, x -> 1, y -> 3}}
  
  are the same map and correspond to the same multinomial (symbolically,
  \eqn{x+3y+4xy^3=4xy^3+x+3y}).  Thus the coefficients of the
  multinomial might be \code{c(1,3,4)} or \code{c(4,1,3)}, or indeed any
  ordering.  But note that any particular ordering imposes an ordering
  on the terms.  If we choose \code{c(1,3,4)} then the terms are
  \code{x,y,xy^3}, and if we choose \code{c(4,1,3)} the terms are
  \code{xy^3,x,y}.

  In the package, \code{disord()} returns an object of class
  \dQuote{\code{mvp_disord}}.  This class of object has a slot for the
  coefficients in the form of a numeric R vector, but also another slot
  which uses hash codes to prevent users from misusing the ordering of
  the numeric vector.
   
  For example, a multinomial \code{x+2y+3z} might have coefficients
  \code{c(1,2,3)} or \code{c(3,1,2)}.  Package idiom to extract the
  coefficients of a multivariate polynomial \code{a} is
  \code{disord(a)}; but this cannot return a standard numeric vector
  because a numeric vector has elements in a particular order, and the
  coefficients of a multivariate polynomial are stored in an
  implementation-specific (and thus unknown) order.
  
  Suppose we have two multivariate polynomials, \code{a} as defined as
  above with \code{a=x+2y+3z} and \code{b=x+3y+4z}.  Even though
  \code{a+b} is well-defined algebraically, and \code{disord(a+b)} will
  return a well-defined \code{mvp_disord} object, idiom such as
  \code{disord(a) + disord(b)} is not defined because there is no
  guarantee that the coefficents of the two multivariate polynomials are
  stored in the same order.  We might have
  \code{c(1,2,3)+c(1,3,4)=c(2,5,7)} or
  \code{c(1,2,3)+c(1,4,3)=c(2,6,6)}, with neither being more
  \dQuote{correct} than the other.  In the package, \code{disord(a) +
  disord(b)} will return an error.  In the same way \code{disord(a) +
  1:3} is not defined and will return an error.  Further, idiom such as
  \code{disord(a) <- 1:3} and \code{disord(a) <- disord(b)} are not
  defined and will return an error.  However, note that \code{disord(a)
  + disord(a)} and \code{disord(a)+disord(a)^2} are fine, these
  returning a \code{mvp_disord} object specific to \code{a}.

  Idiom such as \code{disord(a) <- disord(a)^2} is fine too, for one
  does not need to know the order of the coefficients on either side, so
  long as the order is the same on both sides.  That would translate
  into idiomatic English: \dQuote{the coefficient of each term of
  \code{a} becomes its square}; note that this operation is insensitive
  to the order of coefficients.  The whole shebang is intended to make
  idiom such as \code{disord(a) <- disord(a)\%\%2} possible (so we can
  manipulate polynomials over finite rings, here \eqn{Z/2Z}).

  The replacement methods are defined so that an expression like
  \code{disord(a)[disord(a) > 5] <- 5} works as expected; the English
  idiom would be \dQuote{Replace any coefficient greater than 5 with 5}.

  To fix ideas, consider \code{a <- rmvp(8)}.  Extraction presents
  issues; consider \code{disord(a)<5}.  This object has Boolean elements
  but has the same ordering ambiguity as \code{disord(a)}.  One might
  expect that we could use this to extract elements of \code{disord(a)},
  specifically elements less than 5.  However,
  \code{disord(a)[disord(a)<5]} in isolation is meaningless: what can be
  done with such an object?  However, it makes sense on the left hand
  side of an assignment, as long as the right hand side is a length-one
  vector.  Idiom such as

  \itemize{
    \item\preformatted{disord(a)[disord(a)<5] <- 4+disord(a)[disord(a)<5]}
    \item\preformatted{disord(a) <- pmax(a,3)}
  }

  is algebraically meaningful (\dQuote{Add 4 to any element less than
  5}; \dQuote{coefficients become the pairwise maximum of themselves and
  3}); but these are not implemented because replacement methods in R
  require the RHS to be evaluated; also there is no ready way to check
  for (meaningless) idiom such as \code{disord(a)[disord(a)<5] <-
  4+disord(a)[disord(a)>3]}.  Further note that \code{disord(a) <-
  rev(disord(a))} is disallowed (as indeed is \code{disord(a) <-
  sample(disord(a))}, although arguably the latter should be OK).

  The function \code{Ops.disord()} passes unary and binary arithmetic
  operators to the appropriate specialist function using a system of
  hash code (created by function \code{hash()}) that prevents one from
  misusing the arbitrary ordering of the coefficients returned by
  \code{disord()}.

  So the output of \code{disord(x)} is defined only up to an unknown
  rearrangement.  The same considerations apply to the output of
  \code{vars()}, which returns a list of character vectors in an
  undefined order, and the output of \code{powers()}, which returns a
  numeric list whose elements are in an undefined order.  However, even
  though the order of these three objects is undefined individually,
  their ordering is jointly consistent in the sense that the first
  element of \code{disord(x)} corresponds to the first element of
  \code{vars(x)} and the first element of \code{powers(x)}.  The
  identity of this element is not defined---but whatever it is, the
  first element of all three accessor methods refers to it.

  Note also that a single term (something like \code{4a^3*b*c^6}) has
  the same issue: the variables are not stored in a well-defined order.
  This does not matter because the algebraic value of the term does not
  depend on the order in which the variables appear and this term would
  be equivalent to \code{4b*c^6*a^3}.

  There are use-cases in the \pkg{mvp} package.

}
\author{Robin K. S. Hankin}
\examples{


a <- disord(1:10)
b <- disord(runif(10))

a + (2*a)  # fine
# a + b # error


a <- disord(1:9)




}
\keyword{symbolmath}
