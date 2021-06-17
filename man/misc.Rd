\name{misc}
\alias{misc}
\alias{length}
\alias{length.disord}
\alias{length<-}
\alias{length<-.disord}
\alias{length,disord-method}
\alias{length<-,disord-method}
\alias{rev}
\alias{rev,disord-method}
\alias{rev.disord}
\alias{sort}
\alias{sort,disord-method}
\alias{sort.disord}
\alias{sapply}
\alias{sapply,disord-method}
\alias{sapply.disord}
\title{Miscellaneous functions}
\description{
This page documents various functions that work for disords, and I will
add to these from time to time as I add new functions that make sense
for disord objects.   Functions like \code{sin()} and \code{abs()} work
as expected: they take and return \code{disord} objects.  However, there
are a few functions that are a little more involved:

\itemize{
  \item\code{rev()} reverses its argument and returns a \code{disord}
  object with a reversed hash, which ensures that \code{rev(rev(x))==x}.
  \item\code{sort()} returns a vector of sorted elements
  \item{length()} returns the length of the data component of the
  object.
  \item{sapply(X,f)} returns a disord object which is the result of
  applying \code{f()} to each element of \code{X}.
}
}
\arguments{\item{x}{Object of class \code{disord}}}
\value{Returns a disord}
\author{Robin K. S. Hankin}
\examples{
a <- disord(c(a=1,b=2,c=7))
names(a)
length(a)


# powers() and vars() in the mvp package return lists; see the vignette
# for more discussion.

l <- disord(list(3,6:9,1:10))  
sapply(l,length)

}
