\name{misc}
\alias{misc}
\alias{names}
\alias{names<-}
\alias{names.disord}
\alias{names<-.disord}
\alias{names,disord-method}
\alias{names<-,disord,ANY-method}
\alias{names<-,disord,disord-method}
\alias{length}
\alias{length.disord}
\alias{length<-}
\alias{length<-.disord}
\alias{length,disord-method}
\alias{length<-,disord-method}
\title{Miscellaneous functions}
\description{
This page documents various functions that work for disords, and I will
add to these from time to time as I add new functions that make sense
for disord objects. 
}
\usage{
\S4method{names}{disord}(x)
\usage{\S4method{length}{disord}(x)}}
\arguments{\item{x}{Object of class \code{disord}}}
\value{Returns a disord}
\author{Robin K. S. Hankin}
\examples{
a <- disord(c(a=1,b=2,c=7))
names(a)
length(a)
}