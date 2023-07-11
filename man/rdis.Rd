\name{rdis}
\alias{rdis}
\alias{rdisord}
\alias{rdisordR}
\title{Random disord objects}
\description{Returns a random disord object}
\usage{rdis(n=9)}
\arguments{\item{n}{Set to sample from, as interpreted by \code{sample()}}}
\details{A simple \code{disord} object,  intended as a quick
  \dQuote{get you going} example}
\value{A \code{disord} object.}
\author{Robin K. S. Hankin}
\examples{
rdis()
rdis(99)
rdis(letters)
}
