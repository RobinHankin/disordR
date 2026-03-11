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
#> A disord object with hash f65afea408ea1d8782cf392f957415b08ba28e29 and elements
#> [1] FALSE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE FALSE
#> (in some order)
rdis() > 1000
#> A disord object with hash b5b290a19e0ecce044de56fc5ea6eb5067f91740 and elements
#> [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> (in some order)
```
