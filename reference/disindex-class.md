# Experimental class `"disindex"`

Experimental `disindex` class provides a `disordR`-compliant method for
indexing `disord` objects. The idea is that `which(x)`, where `x` is
Boolean of class `disord`, should have meaning under `disordR`
discipline. Thus [`which()`](https://rdrr.io/r/base/which.html) gives a
`disindex` object. This object can be used as an index for other
`disord` objects. One application would be the `dismat` class of
matrices, currently under development.

Function `values()` coerces its argument to an integer vector.

## Objects from the Class

Objects can be created by calls of the form `new("disindex", ...)`,
although [`which()`](https://rdrr.io/r/base/which.html) is more natural.

## Slots

- `value`::

  Numeric vector

- `hash`::

  Object of class `character` that specifies the hash code

## Author

Robin K. S. Hankin

## Examples

``` r

(x <- disord(c(1,2,1,2,2,7)))
#> A disord object with hash 4632f50c8c2e24e8ea4a2bb96db692e09483722c and elements
#> [1] 1 2 1 2 2 7
#> (in some order)

x==2
#> A disord object with hash 4632f50c8c2e24e8ea4a2bb96db692e09483722c and elements
#> [1] FALSE  TRUE FALSE  TRUE  TRUE FALSE
#> (in some order)
w <- which(x==2)
w
#> A disind object with hash 4632f50c8c2e24e8ea4a2bb96db692e09483722c and 3 (implementation-specific) elements

x[w] <- 100
x
#> A disord object with hash 0af1f55643cfa17ca1631cc6a69ca30e9d8729b8 and elements
#> [1]   1 100   1 100 100   7
#> (in some order)




```
