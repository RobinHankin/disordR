\name{rdisord}
\alias{rdisord}
\title{Random disord object}
\description{A random disord object, intended as a quick
  \dQuote{get you going} example}
\usage{rdisord(n=20)}
\arguments{\item{n}{Number of elements}}

\details{

Function definition is disord(sample(n=20))
\code{a^2*b*a^3} will be simplified to \code{a^5*b}, so powers in the
result may be larger than argument \code{pow}. 
}
\value{
Returns a multivariate polynomial, an object of class \code{mvp}}
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
