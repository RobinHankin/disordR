# Functionality for `disord` objects

Allows arithmetic operators to be used for disord objects; the canonical
application is coefficients of multivariate polynomials (as in the
[mvp](https://CRAN.R-project.org/package=mvp) package). The issue is
that the storage order of disord objects is implementation-specific but
the order (whatever it is) must be consistent between the list of keys
and values in an associative array.

## Usage

``` r
is.disord(x)
hash(x)
hashcal(x,ultra_strict=FALSE)
disord(v,h,drop=TRUE)
elements(x)
```

## Arguments

- x:

  Object of class `disord`

- v:

  Vector of coefficients

- h:

  Hash code

- drop:

  Boolean, with default `FALSE` meaning to return a disord object and
  `TRUE` meaning to call
  [`drop()`](https://robinhankin.github.io/disordR/reference/drop.md)
  before returning

- ultra_strict:

  Boolean, with default `FALSE` meaning to use just `x` to generate the
  hash, and `TRUE` meaning to use the date and a random number as well
  \[this ensures that the hash is generated only once\]

## Details

A detailed vignette is provided that motivates the package. In
applications such as the [mvp](https://CRAN.R-project.org/package=mvp)
or [clifford](https://CRAN.R-project.org/package=clifford) packages, the
user will not need to even think about the
[disordR](https://CRAN.R-project.org/package=disordR) package: it works
in the background. The purpose of the package is to trap plausible idiom
that is ill-defined (implementation-specific) and return an informative
error, rather than returning a possibly incorrect result.

The package provides a single S4 class, `disord`, which has two slots,
`.Data` and `hash`.

Function `disord()` takes an R object such as a vector or list and
returns a `disord` object, which is useful in the context of the `STL`
map class.

Function `hash()` returns the hash of an object (compare `hashcal()`
which is used to actually calculate the hash code).

The package detects acceptable and forbidden operations using hash
codes: function
[`consistent()`](https://robinhankin.github.io/disordR/reference/consistent.md)
checks for its arguments having the same hash code, and thus their
elements can be paired up (e.g. added). Idiomatically, `a %~% b` is
equivalent to `consistent(a,b)`.

Function `elements()` takes a `disord` and returns a regular R object,
typically a vector or a list.

## Value

Boolean, hash code, or object of class `disord` as appropriate.

## Author

Robin K. S. Hankin

## Examples

``` r
(a <- rdis())
#> A disord object with hash a78e8a431889401854b8a6a533b65dfa3276ae78 and elements
#> [1] 1 6 2 3 1 9 2 5 8
#> (in some order)
(b <- rdis())
#> A disord object with hash 0193b1824e2d45940a15932021b5415be83187fe and elements
#> [1] 3 3 2 7 3 6 4 1 7
#> (in some order)

a + 2*a + 2^a  # fine
#> A disord object with hash a78e8a431889401854b8a6a533b65dfa3276ae78 and elements
#> [1]   5  82  10  17   5 539  10  47 280
#> (in some order)
# a + b # this would give an error if executed

a[a<0.5] <- 0       # round down; replacement works as expected

elements(a)
#> [1] 1 6 2 3 1 9 2 5 8
```
