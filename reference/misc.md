# Miscellaneous functions

This page documents various functions that work for disords, and I will
add to these from time to time as I add new functions that make sense
for disord objects (or identify functions that break disord discipline).
Functions like [`sin()`](https://rdrr.io/r/base/Trig.html) and
[`abs()`](https://rdrr.io/r/base/MathFun.html) work as expected: they
take and return `disord` objects with the same hash as `x` (which means
that idiom like `x + sin(x)` is accepted). However, there are a few
functions that are a little more involved:

- `rev()` reverses its argument and returns a `disord` object with a
  reversed hash, which ensures that `rev(rev(x))==x` (and the two are
  consistent).

- `sort()` returns a vector of sorted elements (not a `disord`)

- `length()` returns the length of the data component of the object

- `sapply(X,f)` returns a disord object which is the result of applying
  `f()` to each element of `X`.

- `match(x,table)` should behave as expected but note that if `table` is
  a `disord`, the result is not defined (because it is not known where
  the elements of `x` occur in `table`). Nevertheless `x %in% table` is
  defined and returns a `disord` object.

- `lapply(x,f)` returns `disord(lapply(elements(x),f,...),h=hash(x))`.
  Note that double square bracket extraction, as in `x[[i]]`, is
  disallowed (see `extract.Rd`).

- [`which()`](https://rdrr.io/r/base/which.html) returns a `disind`
  object when given a Boolean `disord`

- `unlist()` takes a `disord` list, flattens it and returns a `disord`
  vector. It requires the `recursive` flag of
  [`base::unlist()`](https://rdrr.io/r/base/unlist.html) to be `TRUE`,
  which it is by default, interpreting this to mean “kill all the
  structure in any sublists”. If the list comprises only length-one
  vectors, the returned value retains the same hash as the argument; if
  not, a new hash is generated.

- `diff()` is undefined for `disord` objects.

- `rbind()` and `cbind()` are undefined for `disord` objects as they
  break disord discipline. Function `binder()` returns a generic, and
  hopefully informative, error message \[the package defines methods for
  `rbind2()` and `cbind2()`\]

- `jitter()` takes a `disord` object, jitters the elements, and returns
  a `disord` object with the correct hash code.

## Arguments

- x:

  Object of class `disord`

## Value

Returns a disord

## Author

Robin K. S. Hankin

## Note

Some functionality is not yet implemented. Factors, lists, and named
vectors do not behave entirely consistently in the package;
[`paste()`](https://rdrr.io/r/base/paste.html) gives inconsistent
results when called with disords.

Also, `for()` loops are incompatible with disord discipline, as they
impose an ordering (`for()` accesses the `.Data` slot of its argument,
which is a regular R vector). Thus:

    > (a <- disord(1:3))
    A disord object with hash 555f6bea49e58a2c2541060a21c2d4f9078c3086 and elements
    [1] 1 2 3
    (in some order)
    > for(i in a){print(i)}
    [1] 1
    [1] 2
    [1] 3
    >

Above, we see that `for()` uses the ordering of the `.Data` slot of S4
object `a`, even though
[`elements()`](https://robinhankin.github.io/disordR/reference/disord.md)
has not been explicitly called.

## See also

[`extract`](https://robinhankin.github.io/disordR/reference/extract.md)

## Examples

``` r
a <- disord(c(a=1,b=2,c=7))
a
#> A disord object with hash 200e179b0d87e371c41f200766ef63fb944fa35a and elements
#> [1] 1 2 7
#> (in some order)
names(a)
#> [1] "a" "b" "c"
length(a)
#> [1] 3
sqrt(a)
#> A disord object with hash 200e179b0d87e371c41f200766ef63fb944fa35a and elements
#> [1] 1.000000 1.414214 2.645751
#> (in some order)


# powers() and vars() in the mvp package return lists; see the vignette
# for more discussion.

l <- disord(list(3,6:9,1:10))  
sapply(l,length)
#> A disord object with hash a56a795922aff5eb253da1f544d642a1c8ebbdd5 and elements
#> [1]  1  4 10
#> (in some order)

unlist(l)
#> A disord object with hash 9107800b5b5fff631c3fd5f16c68dce2825c85bd and elements
#>  [1]  3  6  7  8  9  1  2  3  4  5  6  7  8  9 10
#> (in some order)

## Quick illustration of rev():

revstring <- function(s){paste(rev(unlist(strsplit(s, NULL))),collapse="")}
x <- rdis()
revstring(hash(x)) == hash(rev(x))
#> [1] TRUE
```
