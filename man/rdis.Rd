\name{rdis}
\alias{rdis}
\alias{rdisord}
\alias{rdisordR}
\title{Random disord objects}
\description{Returns a random disord object}
\usage{rdis(n=9)}
\arguments{\item{n}{Number of elements}}
\details{A simple \code{disord} object,  intended as a quick
  \dQuote{get you going} example}
\value{A \code{disord} object.}
\author{Robin K. S. Hankin}
\examples{
rdis()

a <- rdis()
b <- rdis()


## Following are OK:
a
a + 2*a - a^2
a>8
a[a<8]
a[a<6] <-  a[a<6] + 100


## Following forbidden:

# a+b
# a[1:4]
# a[b<4]
# a[a<4] <- 1:4
# a[a<4] <- a[b<4] + 100

}
