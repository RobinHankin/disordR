# Summaries of disord objects

A summary method for disord objects, and a print method for summaries.

## Usage

``` r
# S4 method for class 'disord'
summary(object, ...)
# S4 method for class 'disindex'
summary(object, ...)
# S3 method for class 'summary.disord'
print(x, ...)
```

## Arguments

- object,x:

  Object of class `disord`

- ...:

  Further arguments, currently ignored

## Details

A `summary.disord` object is summary of a `disord` object `x`: a list
with first element being the `hash(x)` and the second being
`summary(elements(x))`. The print method is just a wrapper for this.

## Author

Robin K. S. Hankin

## Examples

``` r
summary(rdis(1000))
#> a disord object with hash de4be24cbe2decd3ae33205e3a597b8f2d83542d 
#> 
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>     4.0   233.5   486.0   490.3   746.2   998.0 
```
