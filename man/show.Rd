\name{show}
\alias{show}
\alias{print}
\alias{print.disord}
\alias{print,disord-method}
\alias{show,disord-method}
\alias{disord_show}
\title{Print method for disord objects}
\description{Show methods for disords}
\usage{
\S4method{show}{disord}(x)
disord_show(x)
}
\arguments{
  \item{x}{Object of class disord}
}
\details{

  The print method simply prints the object's hash and its elements,
  together with a reminder that the elements are listed in an
  implementation-specific order.  Function \code{disord_show()} is a
  helper function, not really intended for the end-user.

}
\author{Robin K. S. Hankin}
\examples{
print(rdis())
}
