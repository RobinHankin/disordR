\name{consistent}
\alias{consistent}
\alias{is.consistent}
\alias{check_matching_hash}
\alias{\%~\%}
\title{Check for consistency}
\description{
  The \pkg{disordR} package is designed to make permitted operations
  transparent and to prevent forbidden operations from being executed.

  Function \code{consistent()} checks for matching hash codes of its
  arguments and returns a Boolean.  It is called by function
  \code{check_matching_hash()} which either returns \code{TRUE} or
  reports an informative error message if not.

}
\usage{
consistent(x,y)
x \%~\% y
check_matching_hash(e1,e2,use=NULL)
}
\arguments{
  \item{x,y,e1,e2}{Objects of class \code{disord}}
  \item{use}{optional object designed to give a more intelligible error
    message; typically \code{match.call()}}
}
\details{
  
  Function \code{consistent()} checks that its arguments have the same
  hash code, and thus their elements can be paired up (e.g. added).
  Idiom \code{a \%~\% b} is equivalent to \code{consistent(a,b)}.

  The package generally checks for consistency with function
  \code{check_matching_hash()} which provides some helpful diagnostics
  if \code{consistent()} finds a hash mismatch.
   
}
\value{Boolean or an error as appropriate}
\author{Robin K. S. Hankin}
\seealso{\code{\link{disord}}}
\examples{

# rdis() + rdis() # this would make check_matching_hash() report an error, if executed

}
\keyword{symbolmath}
