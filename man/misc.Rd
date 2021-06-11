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
\alias{cos}
\alias{cos,disord-method}
\alias{sin}
\alias{sin,disord-method}
\alias{tan}
\alias{tan,disord-method}
\alias{acos}
\alias{acos,disord-method}
\alias{asin}
\alias{asin,disord-method}
\alias{atan}
\alias{atan,disord-method}
\alias{cospi}
\alias{cospi,disord-method}
\alias{sinpi}
\alias{sinpi,disord-method}
\alias{tanpi}
\alias{tanpi,disord-method}
\alias{cosh}
\alias{cosh,disord-method}
\alias{sinh}
\alias{sinh,disord-method}
\alias{tanh}
\alias{tanh,disord-method}
\alias{acosh}
\alias{acosh,disord-method}
\alias{asinh}
\alias{asinh,disord-method}
\alias{atanh}
\alias{atanh,disord-method}
\alias{log}
\alias{log,disord-method}
\alias{log10}
\alias{log10,disord-method}
\alias{log2}
\alias{log2,disord-method}
\alias{log1p}
\alias{log1p,disord-method}
\alias{exp}
\alias{exp,disord-method}
\alias{expm1}
\alias{expm1,disord-method}
\alias{abs}
\alias{abs,disord-method}
\alias{sqrt}
\alias{sqrt,disord-method}
\alias{as.character}
\alias{as.character,disord-method}
\title{Miscellaneous functions}
\description{
This page documents various functions that work for disords, and I will
add to these from time to time as I add new functions that make sense
for disord objects. 
}
\arguments{\item{x}{Object of class \code{disord}}}
\value{Returns a disord}
\author{Robin K. S. Hankin}
\examples{
a <- disord(c(a=1,b=2,c=7))
names(a)
length(a)
}