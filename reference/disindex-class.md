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
#> A disord object with hash 83f9c6dcd135918971c4c4494b1e969e2fa8ebf0 and elements
#> [1] 1 2 1 2 2 7
#> (in some order)

x==2
#> A disord object with hash 83f9c6dcd135918971c4c4494b1e969e2fa8ebf0 and elements
#> [1] FALSE  TRUE FALSE  TRUE  TRUE FALSE
#> (in some order)
w <- which(x==2)
w
#> A disind object with hash 83f9c6dcd135918971c4c4494b1e969e2fa8ebf0 and 3 (implementation-specific) elements

x[w] <- 100
x
#> A disord object with hash 19cb0dd3622934a8c5a1fa05cef49f0bc8f88270 and elements
#> [1]   1 100   1 100 100   7
#> (in some order)




```
