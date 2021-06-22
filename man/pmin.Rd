\name{pmin}
\alias{pmin}
\alias{pmin.disord}
\alias{pmin,disord-method}
\alias{pmindis}
\alias{pmindispair}
\alias{pmindis,disord-method}
\alias{pmindispair,disord,ANY-method}
\alias{pmindispair,disord,disord-method}
\alias{pmindispair,ANY,disord-method}
\alias{pmax}
\alias{pmax.disord}
\alias{pmax,disord-method}
\alias{pmaxdis}
\alias{pmaxdispair}
\alias{pmaxdis,disord-method}
\alias{pmaxdispair,disord,ANY-method}
\alias{pmaxdispair,disord,disord-method}
\alias{pmaxdispair,ANY,disord-method}
\title{Parallel min and max}
\description{
Parallel maxima and minima for disord objects.  It would be nice to have
\code{pmax()} and \code{pmin()} generic functions but this does not seem
to be possible so we have \code{pmaxdis()} and \code{pmindis()} instead.
Even these are not ideal, as idiom such as \code{pmindis(3,disord(1:5))}
[which would want to return \code{disord(c(1,1,1,3,3))}] does not work
as dispatch is determined by the first argument (to make this work,
either coerce, as in \code{pmindis(disord(3),disord(1:5))}, or
rearrange, as in \code{pmindis(disord(1:5),3)}).
}
\value{Object of class \code{disord}.}
\author{Robin K. S. Hankin}
\seealso{\code{\link{misc}}}
\examples{
a <- rdis()
b <- rdis()

# pmindis(a,b)   # fails!

pmindis(a,0.5)
pmindis(a,1-a)

pmaxdis(a+1, a*2, 1.3)
}
\keyword{classes}
