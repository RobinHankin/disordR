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
\alias{lapply}
\alias{lapply,disord-method}
\alias{lapply.disord}
\alias{is.na}
\alias{is.na.disord}
\alias{is.na<-}
\alias{is.na<-.disord}
\alias{is.na,disord-method}
\alias{is.na<-,disord-method}
\alias{match}
\alias{match,disord-method}
\alias{match,disord,ANY-method}
\alias{match,ANY,disord-method}
\alias{match,ANY,disord-method}
\alias{match,disord,disord-method}
\alias{\%in\%}
\alias{\%in\%,disord-method}
\alias{\%in\%,disord,ANY-method}
\alias{\%in\%,ANY,disord-method}
\alias{\%in\%,ANY,disord-method}
\alias{\%in\%,disord,disord-method}
\alias{!,disord-method}
\alias{as.logical,disord-method}
\alias{as.numeric,disord-method}
\alias{as.double,disord-method}
\alias{as.list,disord-method}
\alias{as.character,disord-method}
\alias{as.complex,disord-method}
\title{Miscellaneous functions}
\description{

This page documents various functions that work for disords, and I will
add to these from time to time as I add new functions that make sense
for disord objects.  Functions like \code{sin()} and \code{abs()} work
as expected: they take and return \code{disord} objects with the same
hash as \code{x} (which means that idiom like \code{x + sin(x)} is
accepted).  However, there are a few functions that are a little more
involved:

\itemize{
  \item\code{rev()} reverses its argument and returns a \code{disord}
  object with a reversed hash, which ensures that \code{rev(rev(x))==x}
  (and the two are consistent).
  \item\code{sort()} returns a vector of sorted elements (not a
  \code{disord})
  \item{length()} returns the length of the data component of the
  object.
  \item\code{sapply(X,f)} returns a disord object which is the result of
  applying \code{f()} to each element of \code{X}.
  \item\code{match(x,table)} should behave as expected but note that if
  \code{table} is a \code{disord}, the result is not defined (because it
  is not known where the elements of \code{x} occur in \code{table}).
  Nevertheless \code{x \%in\% table} is defined and returns a
  \code{disord} object.
}

Some functionality is not yet implemented.  Factors, lists, and named
vectors do not behave entirely consistently in the package;
\code{paste()} gives inconsistent results when called with disords.

Also, \code{for()} loops are incompatible with disord discipline, as
they impose an ordering (\code{for()} accesses the \code{.Data} slot of
its argument, which is a regular \R vector).  Thus:

\preformatted{
> (a <- disord(1:3))
A disord object with hash 555f6bea49e58a2c2541060a21c2d4f9078c3086 and elements
[1] 1 2 3
(in some order)
> for(i in a){print(i)}
[1] 1
[1] 2
[1] 3
> 
}

Above, we see that \code{for()} uses the ordering of the \code{.Data}
slot of \proglang{S4} object \code{a}, even though \code{elements()} has
not been explicitly called.

}
\arguments{\item{x}{Object of class \code{disord}}}
\value{Returns a disord}
\author{Robin K. S. Hankin}
\examples{
a <- disord(c(a=1,b=2,c=7))
a
names(a)
length(a)
sqrt(a)


# powers() and vars() in the mvp package return lists; see the vignette
# for more discussion.

l <- disord(list(3,6:9,1:10))  
sapply(l,length)

}
