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
#> A disord object with hash 5b7279f3c05d00cf1e8f999a755151e0451c56ec and elements
#> [1] 9 4 7 1 2 6 3 8 5
#> (in some order)
```

We may perform various operations on this object:

``` r
a+4
#> A disord object with hash 5b7279f3c05d00cf1e8f999a755151e0451c56ec and elements
#> [1] 13  8 11  5  6 10  7 12  9
#> (in some order)
a > 5
#> A disord object with hash 5b7279f3c05d00cf1e8f999a755151e0451c56ec and elements
#> [1]  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
#> (in some order)
a[a<6]
#> A disord object with hash 1cb2c1cb8b5cb3c58e69ffcc5f88340cbdc6abbd and elements
#> [1] 4 1 2 3 5
#> (in some order)
```

with no difficulty. But if we try to find elements of `a` with a
particular offset or offsets, the system returns an error:

``` r
a[1]
#> Error in .local(x, i, j = j, ..., drop): if using a regular index to extract, must extract each element once and once only (or none of them)
a[c(2,3)]
#> Error in .local(x, i, j = j, ..., drop): if using a regular index to extract, must extract each element once and once only (or none of them)
```

This is because the elements of `a` are stored in an
implementation-specific order and there is no such thing as the “first”
element. We may manipulate elements of `a` by reference to their values
but not by their position in the vector:

``` r
a[a<3] <- 0  # round small elements down
a
#> A disord object with hash 5b7279f3c05d00cf1e8f999a755151e0451c56ec and elements
#> [1] 9 4 7 0 0 6 3 8 5
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
#> A disord object with hash 488e1c6f4e2c062379d47b5511730a9785661318 and elements
#> [1] 2 3 8 1 5 6 9 7 4
#> (in some order)
```

Then `a` and `b` have the same length, but adding them vectorially is
forbidden because the order of their elements is
implementation-specific:

``` r
a+b
#> disordR discipline error in:
#> a + b
#> 
#> hash codes 5b7279f3c05d00cf1e8f999a755151e0451c56ec and 488e1c6f4e2c062379d47b5511730a9785661318 do not match
#> Error in check_matching_hash(e1, e2, match.call()): stopping
```

Also, replacement methods that access cross-referenced locations are
forbidden:

``` r
a[b < 4] <- 5
#> disordR discipline error in:
#> .local(x = x, i = i, j = j, value = value)
#> 
#> hash codes 5b7279f3c05d00cf1e8f999a755151e0451c56ec and 488e1c6f4e2c062379d47b5511730a9785661318 do not match
#> Error in check_matching_hash(x, i, match.call()): stopping
```

(observe carefully that `a[b<14] <- 5` is legal). However, sometimes one
knows that two disord objects have the *same* ordering (perhaps `a` is
the key, and `b` the value, of an associative array). In this case one
may cross-reference them provided that the hash codes of the two objects
agree:

``` r
a <- rdis()
b <- disord(sample(9),hash(a))
a
#> A disord object with hash 909ab9cb9afebb8a5109f17b277b37d7e6b1aaa1 and elements
#> [1] 7 1 9 5 6 8 4 2 3
#> (in some order)
b
#> A disord object with hash 909ab9cb9afebb8a5109f17b277b37d7e6b1aaa1 and elements
#> [1] 5 2 8 6 1 4 3 9 7
#> (in some order)
```

See how `a` and `b` have the same hash code. Then, although the elements
of `a` and `b` are stored in an implementation-specific (and thus
unknown) order, whatever order it is is the same in both objects and
they are relatable:

``` r
a+b
#> A disord object with hash 909ab9cb9afebb8a5109f17b277b37d7e6b1aaa1 and elements
#> [1] 12  3 17 11  7 12  7 11 10
#> (in some order)
a[b < 0.5]
#> integer(0)
a[b < 0.2] <- b[b < 0.2]
a
#> A disord object with hash 909ab9cb9afebb8a5109f17b277b37d7e6b1aaa1 and elements
#> [1] 7 1 9 5 6 8 4 2 3
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
