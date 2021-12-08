\name{summary.disordR}
\alias{summary.disordR}
\alias{summary.disord}
\alias{print.summary.disord}
\title{Summaries of disord objects}
\description{
A summary method for disord objects, and a print method for summaries.
}
\usage{
\method{summary}{disord}(object, ...)
\method{print}{summary.disord}(x, ...)
}
\arguments{
  \item{object,x}{Object of class \code{disord}}
  \item{...}{Further arguments, currently ignored}
}
\details{
  A \code{summary.disord} object is summary of a \code{disord} object
  \code{x}: a list with first element being the \code{hash(x)} and the
  second being \code{summary(elements(x))}.  The print method is just a
  wrapper for this.
}
\author{Robin K. S. Hankin}
\examples{
summary(rdis(1000))
}
