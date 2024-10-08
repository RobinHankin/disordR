\name{misc}
\alias{misc}
\alias{length}
\alias{length.disord}
\alias{length.disindex}
\alias{length<-}
\alias{length<-.disord}
\alias{length,disord-method}
\alias{length,disindex-method}
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
\alias{unlist,disord-method}
\alias{which,disord-method}
\alias{which,disindex-method}
\alias{unlist}
\alias{diff}
\alias{binder}
\alias{rbind}
\alias{cbind}
\title{Miscellaneous functions}
\description{

This page documents various functions that work for disords, and I will
add to these from time to time as I add new functions that make sense
for disord objects (or identify functions that break disord discipline).
Functions like \code{sin()} and \code{abs()} work as expected: they take
and return \code{disord} objects with the same hash as \code{x} (which
means that idiom like \code{x + sin(x)} is accepted).  However, there
are a few functions that are a little more involved:

\itemize{
  \item\code{rev()} reverses its argument and returns a \code{disord}
  object with a reversed hash, which ensures that \code{rev(rev(x))==x}
  (and the two are consistent).
  \item\code{sort()} returns a vector of sorted elements (not a
  \code{disord})
  \item{\code{length()} returns the length of the data component of the
  object}
  \item\code{sapply(X,f)} returns a disord object which is the result of
  applying \code{f()} to each element of \code{X}.
  \item\code{match(x,table)} should behave as expected but note that if
  \code{table} is a \code{disord}, the result is not defined (because it
  is not known where the elements of \code{x} occur in \code{table}).
  Nevertheless \code{x \%in\% table} is defined and returns a
  \code{disord} object.
  \item\code{lapply(x,f)} returns
  \code{disord(lapply(elements(x),f,...),h=hash(x))}.  Note that double
  square bracket extraction, as in \code{x[[i]]}, is disallowed (see
  \code{extract.Rd}).
  \item\code{which()} returns a \code{disind} object when given a
  Boolean  \code{disord}
  \item\code{unlist()} takes a \code{disord} list, flattens it and
  returns a \code{disord} vector.  It requires the \code{recursive} flag
  of \code{base::unlist()} to be \code{TRUE}, which it is by default,
  interpreting this to mean \dQuote{kill all the structure in any
  sublists}.  If the list comprises only length-one vectors, the
  returned value retains the same hash as the argument; if not, a new
  hash is generated.
  \item\code{diff()} is undefined for \code{disord} objects.
  \item\code{rbind()} and \code{cbind()} are undefined for \code{disord}
  objects as they break disord discipline.  Function \code{binder()}
  returns a generic, and hopefully informative, error message [the
  package defines methods for \code{rbind2()} and \code{cbind2()}]
}
}
\arguments{\item{x}{Object of class \code{disord}}}
\value{Returns a disord}
\author{Robin K. S. Hankin}
\note{

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
\seealso{\code{\link{extract}}}
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

unlist(l)

## Quick illustration of rev():

revstring <- function(s){paste(rev(unlist(strsplit(s, NULL))),collapse="")}
x <- rdis()
revstring(hash(x)) == hash(rev(x))

}
