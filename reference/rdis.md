# Random disord objects

Returns a random disord object

## Usage

``` r
rdis(n=9)
```

## Arguments

- n:

  Set to sample from, as interpreted by
  [`sample()`](https://rdrr.io/r/base/sample.html)

## Details

A simple `disord` object, intended as a quick “get you going” example

## Value

A `disord` object.

## Author

Robin K. S. Hankin

## Examples

``` r
rdis()
#> A disord object with hash 78478c44b2cf93c422d15eb42ca13979821f9a21 and elements
#> [1] 4 2 9 2 3 3 1 7 5
#> (in some order)
rdis(99)
#> A disord object with hash fd48b871fe4557691e4de431d360599eca12ef2f and elements
#>  [1] 65  3  7 49 70 91 12 39 11 42 40 15 82 44 54 85 58 29 74 67 14 66 69 82 37
#> [26]  4 83 56 52 57 17 87 31 93 52 74 24 55 73 89 10 73 83 89 51 29 49 79 63 23
#> [51] 57 31 59 75 54 40 60  7 51 92 12  3 16 72 12 66 71 82 26 64 33 90  2 73 57
#> [76] 43 33 16 87 15 32 84 91 65 60 25 10 43 95 60 63 58 63 62 34 42 50 37 19
#> (in some order)
rdis(letters)
#> A disord object with hash 57afa8ac0b87103f658fad5a19bb02adc78995cc and elements
#>  [1] "n" "b" "u" "e" "f" "s" "q" "x" "h" "v" "h" "n" "c" "r" "o" "o" "k" "x" "g"
#> [20] "z" "u" "h" "z" "o" "e" "p"
#> (in some order)
```
