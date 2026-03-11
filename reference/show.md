# Print method for disord objects

Show methods for disords

## Usage

``` r
# S4 method for class 'disord'
show(x)
disord_show(x)
```

## Arguments

- x:

  Object of class disord

## Details

The print method simply prints the object's hash and its elements,
together with a reminder that the elements are listed in an
implementation-specific order. Function `disord_show()` is a helper
function, not really intended for the end-user.

## Author

Robin K. S. Hankin

## Examples

``` r
print(rdis())
#> A disord object with hash 75b39653f6e276f2ffb0b9e74ae6dd62bbb97de1 and elements
#> [1] 4 1 9 2 7 3 8 3 1
#> (in some order)
```
