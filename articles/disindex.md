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

    ## A disord object with hash add9750766cabeff2f6f38f1a5cfacb1b2ab162d and elements
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

    ## A disord object with hash add9750766cabeff2f6f38f1a5cfacb1b2ab162d and elements
    ## [1] 4 6 1 2 3 4 5 1
    ## (in some order)

``` r

d[ind]
```

    ## A disord object with hash e9d1a41267931b1fed19f519aa9c5bea8e487314 and elements
    ## [1] 6 5
    ## (in some order)

``` r

d[ind] <- 99
d
```

    ## A disord object with hash c1b110f77a0e952a149e1cac86ff02acd0633eb1 and elements
    ## [1]  4 99  1  2  3  4 99  1
    ## (in some order)

However, we cannot assert that `ind` is elements 2 and 7 of `d`, for the
elements of `d` are stored in an implementation-specific order. If we
examine `ind` directly, we see:

``` r

ind
```

    ## A disind object with hash add9750766cabeff2f6f38f1a5cfacb1b2ab162d and 2 (implementation-specific) elements

which correctly says that the elements of `ind` are
implementation-specific. However, the main application of `disindex`
objects is for list extraction.

``` r

d <- disord(c(4,1,6,2))
dl <- sapply(d,function(x){seq(from=5,to=x)})
dl
```

    ## A disord object with hash 5b13272b70b93e1d46866c494544fc1321409d1d and elements
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
