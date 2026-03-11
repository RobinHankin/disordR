# Check for consistency

The disordR package is designed to make permitted operations transparent
and to prevent forbidden operations from being executed.

Function `consistent()` checks for matching hash codes of its arguments
and returns a Boolean. It is called by function `check_matching_hash()`
which either returns `TRUE` or reports an informative error message if
not.

## Usage

``` r
consistent(x,y)
x %~% y
check_matching_hash(e1,e2,use=NULL)
```

## Arguments

- x,y,e1,e2:

  Objects of class `disord`

- use:

  optional object designed to give a more intelligible error message;
  typically [`match.call()`](https://rdrr.io/r/base/match.call.html)

## Details

Function `consistent()` checks that its arguments have the same hash
code, and thus their elements can be paired up (e.g. added). Idiom
`a %~% b` is equivalent to `consistent(a,b)`.

The package generally checks for consistency with function
`check_matching_hash()` which provides some helpful diagnostics if
`consistent()` finds a hash mismatch.

## Value

Boolean or an error as appropriate

## Author

Robin K. S. Hankin

## See also

`disord`

## Examples

``` r
# rdis() + rdis() # this would make check_matching_hash() report an error, if executed
```
