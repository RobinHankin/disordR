\name{rdisord}
\alias{rdisord}
\title{Random disord object}
\description{A random disord object}
\usage{rdisord(n=20)}
\arguments{\item{n}{Number of elements}}
\details{A simple \code{disord} object,  intended as a quick
  \dQuote{get you going} example}
\value{A \code{disord} object.}
\author{Robin K. S. Hankin}
\examples{
rdisord()

a <- rdisord()
b <- rdisord()


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
