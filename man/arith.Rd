\name{Arith}
\docType{methods}
\alias{Arith}
\alias{disord_inverse}
\alias{disord_negative}
\alias{disord_positive}
\alias{disord_mod_disord}
\alias{disord_mod_numeric}
\alias{numeric_mod_disord}
\alias{disord_plus_disord}
\alias{disord_plus_numeric}
\alias{disord_power_disord}
\alias{disord_power_numeric}
\alias{numeric_power_disord}
\alias{disord_prod_disord}
\alias{disord_prod_numeric}
\alias{any_compare_disord}
\alias{disord_arith_disord}
\alias{disord_arith_numeric}
\alias{disord_compare_any}
\alias{disord_compare_disord}
\alias{disord_logic}
\alias{disord_show}
\alias{disord_unary}
\alias{numeric_arith_disord}
\title{Arithmetic operations}
\description{Arithmetic operations including low-level helper functions}
\usage{
disord_inverse(a)
disord_mod_disord(a,b)
disord_mod_numeric(a,b)
disord_negative(a)
disord_plus_disord(a,b)
disord_plus_numeric(a,b)
disord_power_disord(a,b)
disord_power_numeric(a,b)
numeric_power_disord(a,b)
disord_prod_disord(a,b)
disord_prod_numeric(a,b)
}
\arguments{\item{a,b}{at least one is a disord object}}
\details{
Basic low-level arithmetic operations, intended to be called from
\proglang{S4} dispatch.

These functions return a \code{disord} object or a regular vector as
appropriate.  Consistency is required.  The hash is set to be that of
the disord object if appropriate.

}
\value{Return a disord object or logical}
\author{Robin K. S. Hankin}
\examples{
a <- disord(sample(30))
b <- disord(sample(30))

a + 2*a

a[a\%\%2==1] <- a[a\%\%2==1] + 5
}