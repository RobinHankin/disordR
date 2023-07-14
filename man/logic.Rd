\name{Logic}
\docType{methods}
\alias{Logic}
\alias{disord_logic}
\alias{disord_logic_disord}
\alias{disord_logic_any}
\alias{any_logic_disord}
\alias{disord_logic_unary}
\alias{disord_logic_missing}
\alias{disord_logical_negate}
\title{Logical operations}
\description{Logical operations including low-level helper functions}
\usage{
disord_logical_negate(x)
disord_logic_disord(e1,e2)
disord_logic_any(e1,e2)
any_logic_disord(e1,e2)
}
\arguments{
  \item{e1,e2,x }{Formal arguments for \proglang{S4} dispatch: logical
    \code{disord} object}
}
\section{Methods}{
  \describe{
    \item{Logic}{\code{signature(e1="disord", e2="disord")}:
      Dispatched to \code{disord_logic_disord()}}
    \item{Logic}{\code{signature(e1="disord", e2="ANY")}:
      Dispatched to \code{disord_logic_any()}}
    \item{Logic}{\code{signature(e1="ANY", e2="disord")}:
      Dispatched to \code{any_logic_disord()}}
  }
}

\details{

Basic low-level logical operations, intended to be called from
\proglang{S4} dispatch.

These functions return a logical \code{disord} object.  appropriate.
Consistency is required.  The hash is set to be that of the disord
object if appropriate.

}
\value{Return a disord object or logical}
\author{Robin K. S. Hankin}
\examples{

a <- disord(1:7)
l <- a>3
sum(l)
any(l)
all(l | !l)

}
