# Drop redundant information

Coerce `disord` objects to vector when this makes sense

## Usage

``` r
drop(x)
allsame(x)
```

## Arguments

- x:

  `disord` object

## Details

If one has a disord object all of whose elements are identical, one
usually wants to drop the disord attribute and coerce to a vector. This
can be done without breaking `disordR` discipline. Function `disord()`
takes a `drop` argument, defaulting to `TRUE`, which drops the `disord`
class from its return value if all the elements are the same.

Similarly, function `drop()` takes a disord object and if all elements
are identical it returns the elements in the form of a vector. Some
extraction methods take a `drop` argument, which does the same thing if
`TRUE`. This is only useful for disord objects created with
`disord(...,drop=FALSE)`

The `drop` functionality is conceptually similar to the `drop` argument
of base R's array extraction, as in

         a <- matrix(1:30,5,6)
         a[1,,drop=TRUE]
         a[1,,drop=FALSE]
       

Function `allsame()` takes a vector and returns `TRUE` if all elements
are identical.

## Value

Function `drop()` returns either a vector or object of class `disord` as
appropriate; `allsame()` returns a Boolean.

## Author

Robin K. S. Hankin

## Examples

``` r
disord(c(3,3,3,3,3))             # default is drop=TRUE
#> [1] 3 3 3 3 3
disord(c(3,3,3,3,3),drop=FALSE)  # retains disord class
#> A disord object with hash 24f35d7aa53f843d8747136c5b3604a75ef719fd and elements
#> [1] 3 3 3 3 3
#> (in some order)

drop(disord(c(3,3,3,3),drop=FALSE)) 
#> [1] 3 3 3 3

## In extraction, argument drop discards disorderliness when possible:
a <- rdis()
a
#> A disord object with hash c1662cee8f28f6130bed6db7a07533f7eee1d2ba and elements
#> [1] 2 8 1 1 7 6 9 2 6
#> (in some order)
a[] <- 6 # a becomes a vector
a
#> [1] 6 6 6 6 6 6 6 6 6
```
