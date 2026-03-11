# Arithmetic operations

Arithmetic operations including low-level helper functions

## Usage

``` r
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
disord_arith_unary(e1,e2)
disord_arith_disord(e1,e2)
disord_arith_numeric(e1,e2)
numeric_arith_disord(e1,e2)
```

## Arguments

- a,b:

  at least one is a disord object

- e1,e2:

  Formal arguments for S4 dispatch

## Details

Basic low-level arithmetic operations, intended to be called from S4
dispatch.

These functions return a `disord` object or a regular vector as
appropriate. Consistency is required. The hash is set to be that of the
disord object if appropriate.

## Methods

- Arith:

  `signature(e1="disord", e2="disord")`: Dispatched to
  `disord_arith_disord()`

- Arith:

  `signature(e1="disord", e2="numeric")`: Dispatched to
  `disord_arith_numeric()`

- Arith:

  `signature(e1="numeric", e2="disord")`: Dispatched to
  `numeric_arith_disord()`

- Arith:

  `signature(e1="disord", e2="missing")`: Dispatched to
  `disord_arith_unary()`

## Value

Return a disord object or logical

## Author

Robin K. S. Hankin

## Examples

``` r
a <- rdis()
a
#> A disord object with hash 0732fb9395bc107472adc18d76dacea096d95bd5 and elements
#> [1] 7 5 6 4 6 9 5 5 8
#> (in some order)
a + 2*a
#> A disord object with hash 0732fb9395bc107472adc18d76dacea096d95bd5 and elements
#> [1] 21 15 18 12 18 27 15 15 24
#> (in some order)
a > 5
#> A disord object with hash 0732fb9395bc107472adc18d76dacea096d95bd5 and elements
#> [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE FALSE  TRUE
#> (in some order)
a[a > 5] <- a[a > 5] + 100
a
#> A disord object with hash 0732fb9395bc107472adc18d76dacea096d95bd5 and elements
#> [1] 107   5 106   4 106 109   5   5 108
#> (in some order)
```
