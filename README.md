The disordR package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/disordR.png" width = "150" align="right" />

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/disordR)](https://cran.r-project.org/package=disordR)
[![Codecov test
coverage](https://codecov.io/gh/RobinHankin/disordR/branch/master/graph/badge.svg)](https://codecov.io/gh/RobinHankin/disordR/branch/master)
<!-- badges: end -->

# Overview

Ordinary R vectors are unsuitable for working with values of associative
maps because elements of an R vector may be accessed by reference to
their location in the vector, but associative maps are stored in
arbitrary order. However, when associating keys with values one needs
both parts to be in 1-1 correspondence, so one cannot dispense with the
order entirely. The disordR package includes a single `S4` class,
`disord`. This class allows one to perform only those operations
appropriate for manipulating values (or keys) of associative maps.

A useful heuristic is that one is only allowed to access or modify a
disord object using a python list comprehension. The idea is to prevent
“illegal” operations on values (or keys) of associative maps, whose
order is undefined or at best implementation-specific, while allowing
sensible operations.

# The `disord` package in use

I will illustrate the package by a few examples of legal and illegal
code. First create a simple `disord` object:

``` r
set.seed(0)
a <- rdis()    # a random disord object
a
#> A disord object with hash 5ebacdc1b1f8f614c40e83e0e70ded255e386bea and elements
#> [1] 0.8966972 0.2655087 0.3721239 0.5728534 0.9082078 0.2016819 0.8983897
#> [8] 0.9446753 0.6607978
#> (in some order)
```

We may perform various operations on this object:

``` r
a+4
#> A disord object with hash 5ebacdc1b1f8f614c40e83e0e70ded255e386bea and elements
#> [1] 4.896697 4.265509 4.372124 4.572853 4.908208 4.201682 4.898390 4.944675
#> [9] 4.660798
#> (in some order)
a > 0.5
#> A disord object with hash 5ebacdc1b1f8f614c40e83e0e70ded255e386bea and elements
#> [1]  TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
#> (in some order)
a[a<0.2]
#> A disord object with hash 2a520942c9be99a9a6f25ace481cc0c0942c617d and elements
#> numeric(0)
#> (in some order)
```

with no difficulty. But if we try to find elements of `a` with a
particular offset or offsets, the system returns an error:

``` r
a[1]
#> Error in .local(x, i, j = j, ..., drop): if using a regular index to extract, must extract each element once and once only
a[c(2,3)]
#> Error in .local(x, i, j = j, ..., drop): if using a regular index to extract, must extract each element once and once only
```

This is because the elements of `a` are stored in an
implementation-specific order and there is no such thing as the “first”
element. We may manipulate elements of `a` by reference to their values
but not by their position in the vector:

``` r
a[a<0.1] <- 0  # round small elements down
a
#> A disord object with hash 5ebacdc1b1f8f614c40e83e0e70ded255e386bea and elements
#> [1] 0.8966972 0.2655087 0.3721239 0.5728534 0.9082078 0.2016819 0.8983897
#> [8] 0.9446753 0.6607978
#> (in some order)
```

Replacement methods can access subsets where this makes sense:

``` r
x <- disord(1:10)
x
#> A disord object with hash 65e11d78de79b7f584068ad856749e3748cb837c and elements
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> (in some order)
x[x<3] <- x[x<3] + 100
x
#> A disord object with hash 65e11d78de79b7f584068ad856749e3748cb837c and elements
#>  [1] 101 102   3   4   5   6   7   8   9  10
#> (in some order)
```

## Two distinct `disord` objects

If we create another `disord` object, `b`:

``` r
b <- rdis()
b
#> A disord object with hash ac586d2c83d7942aeb32d50d64c333e631f6912f and elements
#> [1] 0.62911404 0.06178627 0.20597457 0.17655675 0.68702285 0.38410372 0.76984142
#> [8] 0.49769924 0.71761851
#> (in some order)
```

Then `a` and `b` have the same length, but adding them vectorially is
forbidden because the order of their elements is
implementation-specific:

``` r
a+b
#> Error in a + b: consistent(e1, e2) is not TRUE
```

Also, replacement methods that access cross-referenced locations are
forbidden:

``` r
a[b < 0.4] <- 5
#> Error in .local(x, i, j = j, ..., value): consistent(x, i) is not TRUE
```

(observe carefully that `a[b<4] <- 5` is legal). However, sometimes one
knows that two disord objects have the *same* ordering (perhaps `a` is
the key, and `b` the value, of an associative array). In this case one
may cross-reference them provided that the hash codes of the two objects
agree:

``` r
a <- rdis()
b <- disord(runif(9),hash(a))
a
#> A disord object with hash e6cd849ae088b5cd33fec9ef3aadae14003e3ab9 and elements
#> [1] 0.9919061 0.3800352 0.7774452 0.9347052 0.2121425 0.6516738 0.1255551
#> [8] 0.2672207 0.3861141
#> (in some order)
b
#> A disord object with hash e6cd849ae088b5cd33fec9ef3aadae14003e3ab9 and elements
#> [1] 0.01339033 0.38238796 0.86969085 0.34034900 0.48208012 0.59956583 0.49354131
#> [8] 0.18621760 0.82737332
#> (in some order)
```

See how `a` and `b` have the same hash code. Then, although the elements
of `a` and `b` are stored in an implementation-specific (and thus
unknown) order, whatever order it is is the same in both objects and
they are relatable:

``` r
a+b
#> A disord object with hash e6cd849ae088b5cd33fec9ef3aadae14003e3ab9 and elements
#> [1] 1.0052964 0.7624231 1.6471361 1.2750542 0.6942226 1.2512396 0.6190964
#> [8] 0.4534383 1.2134874
#> (in some order)
a[b < 0.5]
#> A disord object with hash c953b2ab2cf81ee8b581dec44ee805f54da7888b and elements
#> [1] 0.9919061 0.3800352 0.9347052 0.2121425 0.1255551 0.2672207
#> (in some order)
a[b < 0.2] <- b[b < 0.2]
a
#> A disord object with hash e6cd849ae088b5cd33fec9ef3aadae14003e3ab9 and elements
#> [1] 0.01339033 0.38003518 0.77744522 0.93470523 0.21214252 0.65167377 0.12555510
#> [8] 0.18621760 0.38611409
#> (in some order)
```

# Comparison with python

A disord object is comparable with python arrays with the proviso that
one is only allowed to access or manipulate elements via list
comprehensions. See the following python session:

    ~ % python3
    Python 3.9.5 (default, May  4 2021, 03:33:11) 
    [Clang 12.0.0 (clang-1200.0.32.29)] on darwin
    Type "help", "copyright", "credits" or "license" for more information.
    >>> from numpy import *
    >>> a = random.rand(5)
    >>> b = random.rand(5)
    >>> a
    array([0.13275574, 0.07243463, 0.0543058 , 0.36633955, 0.73795801])
    >>> b
    array([0.14934221, 0.11321413, 0.6135964 , 0.53558058, 0.6396702 ])
    >>> [i**2 for i in a]
    [0.017624086607707354, 0.005246776214236293, 0.002949120389839953, 0.1342046637421116, 0.54458202262916]
    >>> [i+j for i,j in zip(a,b)]
    [0.28209795105056, 0.18564876373593042, 0.6679022004516395, 0.901920128499422, 1.377628206483919]
    >>> 

Note that in the last line `zip()` is used to relate `a` and `b` which
is accomplished in the package by explicitly setting the hash code.
