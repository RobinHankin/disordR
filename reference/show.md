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
#> A disord object with hash 3c0e6469f221961aa6cd77f242c27b3df3dc6656 and elements
#> [1] 4 1 9 2 7 3 8 3 1
#> (in some order)

a <- rdis()
print(a)
#> A disord object with hash da99567672fcc26e1ffe10306c932aca75049cba and elements
#> [1] 1 9 5 2 8 2 4 5 7
#> (in some order)
print(a+100)  # same hash code
#> A disord object with hash da99567672fcc26e1ffe10306c932aca75049cba and elements
#> [1] 101 109 105 102 108 102 104 105 107
#> (in some order)
```
