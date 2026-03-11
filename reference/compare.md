# Methods for comparison of `disord` objects

Arithmetic comparison methods (greater than, etc) for `disord` objects.

## Methods

- Compare:

  `signature(e1="disord", e2="disord")`: Dispatched to
  `disord_compare_disord()`

- Compare:

  `signature(e1="disord", e2="ANY")`: Dispatched to
  `disord_compare_any()`

- Compare:

  `signature(e1="ANY", e2="disord")`: Dispatched to
  `any_compare_disord()`

## Note

All the comparison methods use `drop=TRUE` to avoid inconsistent results
when all the values are the same \[that is, all `TRUE` or all `FALSE`\].
Comparing two `disord` objects requires their hash code to agree as per
disordR discipline. Comparing a `disord` with a numeric returns a
`disord` object. In each case, the hash code of the original object is
preserved in the returned value.

## Examples

``` r
rdis() > 4
#> A disord object with hash 5e418a960f24d262868d8455cab5f84591f08122 and elements
#> [1] FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE
#> (in some order)
rdis() > 1000
#> A disord object with hash 65a1ea4ed3190df2f415d258ea415a41ecc4e7bf and elements
#> [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> (in some order)
```
