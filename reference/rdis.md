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
#> A disord object with hash de1962125e06f9a5bc31d93f0ccd39e18ec81421 and elements
#> [1] 4 2 9 2 3 3 1 7 5
#> (in some order)
rdis(99)
#> A disord object with hash 835178375be55af2a94bb1d48e2250c66ac2a16e and elements
#>  [1] 65  3  7 49 70 91 12 39 11 42 40 15 82 44 54 85 58 29 74 67 14 66 69 82 37
#> [26]  4 83 56 52 57 17 87 31 93 52 74 24 55 73 89 10 73 83 89 51 29 49 79 63 23
#> [51] 57 31 59 75 54 40 60  7 51 92 12  3 16 72 12 66 71 82 26 64 33 90  2 73 57
#> [76] 43 33 16 87 15 32 84 91 65 60 25 10 43 95 60 63 58 63 62 34 42 50 37 19
#> (in some order)
rdis(letters)
#> A disord object with hash 4af23b5b931c876c3db640ee552410f668b7c93a and elements
#>  [1] "n" "b" "u" "e" "f" "s" "q" "x" "h" "v" "h" "n" "c" "r" "o" "o" "k" "x" "g"
#> [20] "z" "u" "h" "z" "o" "e" "p"
#> (in some order)
```
