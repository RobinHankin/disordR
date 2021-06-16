The disordR package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

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
#> A disord object with hash 1859a5af5f32b6607de2b44c0b10303ebed8e206 and elements
#>  [1] 0.8966972 0.2655087 0.3721239 0.5728534 0.9082078 0.2016819 0.8983897
#>  [8] 0.9446753 0.6607978 0.6291140
#> (in some order)
```

We may perform various operations on this object:

``` r
a+4
#> A disord object with hash 1859a5af5f32b6607de2b44c0b10303ebed8e206 and elements
#>  [1] 4.896697 4.265509 4.372124 4.572853 4.908208 4.201682 4.898390 4.944675
#>  [9] 4.660798 4.629114
#> (in some order)
a > 0.5
#> A disord object with hash 1859a5af5f32b6607de2b44c0b10303ebed8e206 and elements
#>  [1]  TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
#> (in some order)
a[a<0.2]
#> A disord object with hash 6e539ed2361b72f824b91a9592ef03d725fce903 and elements
#> numeric(0)
#> (in some order)
```

with no difficulty. But if we try to find elements of `a` with a
particular offset or offsets, the system returns an error:

``` r
a[1]
#> Error in .local(x, i, j = j, ..., drop): cannot use a regular index to extract, only a disord object
a[c(2,3)]
#> Error in .local(x, i, j = j, ..., drop): cannot use a regular index to extract, only a disord object
```

This is because the elements of `a` are stored in an
implementation-specific order and there is no such thing as the “first”
element. We may manipulate elements of `a` by reference to their values
but not by their position in the vector:

``` r
a[a<0.1] <- a[a<0.1] * 0  # round small elements down
a
#> A disord object with hash 1859a5af5f32b6607de2b44c0b10303ebed8e206 and elements
#>  [1] 0.8966972 0.2655087 0.3721239 0.5728534 0.9082078 0.2016819 0.8983897
#>  [8] 0.9446753 0.6607978 0.6291140
#> (in some order)
```

## Two distinct `disord` objects

If we create another `disord` object, `b`:

``` r
b <- rdis()
b
#> A disord object with hash b46268aa3b44c2c31279a2283b11525dd790e000 and elements
#>  [1] 0.06178627 0.20597457 0.17655675 0.68702285 0.38410372 0.76984142
#>  [7] 0.49769924 0.71761851 0.99190609 0.38003518
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
b <- disord(runif(10),hash(a))
a
#> A disord object with hash c85bd325f942f143eb4065d92d6461dcf08b964a and elements
#>  [1] 0.77744522 0.93470523 0.21214252 0.65167377 0.12555510 0.26722067
#>  [7] 0.38611409 0.01339033 0.38238796 0.86969085
#> (in some order)
b
#> A disord object with hash c85bd325f942f143eb4065d92d6461dcf08b964a and elements
#>  [1] 0.3403490 0.4820801 0.5995658 0.4935413 0.1862176 0.8273733 0.6684667
#>  [8] 0.7942399 0.1079436 0.7237109
#> (in some order)
```

See how `a` and `b` have the same hash code. Then, although the elements
of `a` and `b` are stored in an implementation-specific (and thus
unknown) order, whatever order it is is the same in both objects and
they are relatable:

``` r
a+b
#> A disord object with hash c85bd325f942f143eb4065d92d6461dcf08b964a and elements
#>  [1] 1.1177942 1.4167853 0.8117083 1.1452151 0.3117727 1.0945940 1.0545808
#>  [8] 0.8076302 0.4903316 1.5934018
#> (in some order)
a[b < 0.5]
#> A disord object with hash 4977d35d7fb0cf7a634c8ab4503d4cc920818d54 and elements
#> [1] 0.7774452 0.9347052 0.6516738 0.1255551 0.3823880
#> (in some order)
a[b < 0.2] <- b[b < 0.2]
a
#> A disord object with hash c85bd325f942f143eb4065d92d6461dcf08b964a and elements
#>  [1] 0.77744522 0.93470523 0.21214252 0.65167377 0.18621760 0.26722067
#>  [7] 0.38611409 0.01339033 0.10794363 0.86969085
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
