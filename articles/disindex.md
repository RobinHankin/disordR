# Disordered indices with the \`disordR\` package: an introduction to class \`disindex\`

![](../../../_temp/Library/disordR/help/figures/disordR.png)

Experimental `S4` class `disindex` allows extraction methods, including
list extraction, to operate with the output of
[`which()`](https://rdrr.io/r/base/which.html). Consider the following R
session:

``` r
library("disordR")
(d <- disord(c(4,6,1,2,3,4,5,1)))
```

    ## A disord object with hash 0149a86d892f6cce3ebf215d415c590e6a80f809 and elements
    ## [1] 4 6 1 2 3 4 5 1
    ## (in some order)

``` r
ind <- which(d>4)
```

Above, object `ind` points to those elements of `d` which exceed 4.
Thus:

``` r
d
```

    ## A disord object with hash 0149a86d892f6cce3ebf215d415c590e6a80f809 and elements
    ## [1] 4 6 1 2 3 4 5 1
    ## (in some order)

``` r
d[ind]
```

    ## A disord object with hash a8001f07325d675fcd125a7680f3f0526e4ca408 and elements
    ## [1] 6 5
    ## (in some order)

``` r
d[ind] <- 99
d
```

    ## A disord object with hash 5785545721dd403383321e49cf972e6fb7dad388 and elements
    ## [1]  4 99  1  2  3  4 99  1
    ## (in some order)

However, we cannot assert that `ind` is elements 2 and 7 of `d`, for the
elements of `d` are stored in an implementation-specific order. If we
examine `ind` directly, we see:

``` r
ind
```

    ## A disind object with hash 0149a86d892f6cce3ebf215d415c590e6a80f809 and 2 (implementation-specific) elements

which correctly says that the elements of `ind` are
implementation-specific. However, the main application of `disindex`
objects is for list extraction.

``` r
d <- disord(c(4,1,6,2))
dl <- sapply(d,function(x){seq(from=5,to=x)})
dl
```

    ## A disord object with hash 4a2a3342fb7817af30fe12f1dffece02a3cc849e and elements
    ## [[1]]
    ## [1] 5 4
    ## 
    ## [[2]]
    ## [1] 5 4 3 2 1
    ## 
    ## [[3]]
    ## [1] 5 6
    ## 
    ## [[4]]
    ## [1] 5 4 3 2
    ## 
    ## (in some order)

Suppose I wish to extract from object `dl` just the element with the
longest length. Noting that this would be a `disord`-compliant question,
we would use:

``` r
howlong <- unlist(lapply(dl,length))
longest <- which(howlong == max(howlong))
dl[[longest]]
```

    ## [1] 5 4 3 2 1
