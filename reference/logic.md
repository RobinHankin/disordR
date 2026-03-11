# Logical operations

Logical operations including low-level helper functions

## Usage

``` r
disord_logical_negate(x)
disord_logic_disord(e1,e2)
disord_logic_any(e1,e2)
any_logic_disord(e1,e2)
```

## Arguments

- e1,e2,x :

  Formal arguments for S4 dispatch: logical `disord` object

## Methods

- Logic:

  `signature(e1="disord", e2="disord")`: Dispatched to
  `disord_logic_disord()`

- Logic:

  `signature(e1="disord", e2="ANY")`: Dispatched to `disord_logic_any()`

- Logic:

  `signature(e1="ANY", e2="disord")`: Dispatched to `any_logic_disord()`

## Details

Basic low-level logical operations, intended to be called from S4
dispatch.

These functions return a logical `disord` object. appropriate.
Consistency is required. The hash is set to be that of the disord object
if appropriate.

## Value

Return a disord object or logical

## Author

Robin K. S. Hankin

## Examples

``` r
a <- disord(1:7)
l <- a>3
sum(l)
#> [1] 4
any(l)
#> [1] TRUE
all(l | !l)
#> [1] TRUE
```
