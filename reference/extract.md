# Extraction and replacement methods for class `"disord"`

The `disord` class provides basic arithmetic and extract/replace methods
for disord objects.

Class index is taken from the excellent
[Matrix](https://CRAN.R-project.org/package=Matrix) package and is a
`setClassUnion()` of classes `numeric`, `logical`, and `character`.

## Methods

- \[:

  `signature(x = "disord", i = "ANY", j = "ANY")`: ...

- \[:

  `signature(x = "disord", i = "index", j = "index")`: ...

- \[:

  `signature(x = "disord", i = "index", j = "missing")`: ...

- \[:

  `signature(x = "disord", i = "missing", j = "index")`: ...

- \[:

  `signature(x = "disord", i = "missing", j = "missing")`: ...

- \[:

  `signature(x = "disord", i = "matrix", j = "missing")`: ...

- \[\<-:

  `signature(x = "disord", i = "index", j = "index")`: ...

- \[\<-:

  `signature(x = "disord", i = "index", j = "missing")`: ...

- \[\<-:

  `signature(x = "disord", i = "missing", j = "index")`: ...

- \[\<-:

  `signature(x = "disord", i = "matrix", j = "missing")`: ...

- \[\<-:

  `signature(x = "disord", i = "missing", j = "missing")`: ...

- \[\[:

  `signature(x = "disord", i = "index")`: ...

- \[\[\<-:

  `signature(x = "disord", i = "index",value="ANY")`: ...

- \[:

  `signature(x="disord",i="disindex",j="missing",drop="ANY")`: ...

- \[:

  `signature(x="disord",i="disindex",j="ANY",drop="ANY")`: ...

- \[:

  `signature(x="ANY",i="disindex",j="ANY",drop="ANY")`: ...

- \[:

  `signature(x="disord",i="disindex",j="missing",value="ANY")`: ...

- \[:

  `signature(x="disord",i="disindex",j="ANY",value="ANY")`: ...

- \[\<-:

  `signature(x="disord",i="disindex",j="missing",drop="ANY")`: ...

- \[\[:

  `signature("disord",i="disindex")`: ...

- \[\[:

  `signature("ANY",i="disindex")`: ...

- \[\[\<-:

  `signature(x="disord",i="disindex",j="missing",value="ANY")` ...

- \[\[\<-:

  `signature(x="ANY",i="disindex",j="ANY",value="ANY")` ...

The extraction method takes a `drop` argument which if `TRUE`, returns
the [`drop()`](https://robinhankin.github.io/disordR/reference/drop.md)
of its value. Extraction, as in `x[i]`, is rarely useful. It is only
defined if one extracts either all, or none, of the elements: anything
else is undefined. Note that the hash code is unchanged if all elements
are extracted (because the order might have changed) but unchanged if
none are (because there is only one way to extract no elements).

Missing arguments for extraction and replacement are slightly
idiosyncratic. Extraction idiom such as `x[]` returns an object
identical to `x` except for the hash code, which is changed. I can't
quite see a sensible use-case for this, but the method allows one to
define an object `y <- x[]` for which `x` and `y` are incompatible.
Replacement idiom `x[] <- v` always coerces to a vector.

Double square extraction, as in `x[[i]]` and `x[[i]] <- value`, is via
(experimental) `disindex` functionality.

## Author

Robin K. S. Hankin

## Note

Package versions prior to `disordR_0.0-9-6` allowed idiom such as

        a <- disord(1:9)
        a[a<3] + a[a>7]
      

but this is now disallowed. The issue is discussed in
`inst/note_on_extraction.Rmd`.

## See also

[`drop`](https://robinhankin.github.io/disordR/reference/drop.md),[`misc`](https://robinhankin.github.io/disordR/reference/misc.md)

## Examples

``` r
a <- disord(sample(9))
a
#> A disord object with hash 5abdaaaaae01d117b9d3e54e2e8dfa68fad4e4c8 and elements
#> [1] 6 9 5 3 4 1 7 8 2
#> (in some order)
a + 6*a^2
#> A disord object with hash 5abdaaaaae01d117b9d3e54e2e8dfa68fad4e4c8 and elements
#> [1] 222 495 155  57 100   7 301 392  26
#> (in some order)
a[a>5]  # "give me all elements of a that exceed 5"
#> A disord object with hash 589e9524ba8e52dc0f34138ab818507c56c351fd and elements
#> [1] 6 9 7 8
#> (in some order)

a[] # a disord object, same elements as 'a', but with a different hash
#> A disord object with hash e4132e84bd63411bfd857cadb0afbffbb091dbf2 and elements
#> [1] 6 9 5 3 4 1 7 8 2
#> (in some order)

a[a<5] <- a[a<5] + 100  # "replace all elements of 'a' less than 5 with their value plus 100"
a
#> A disord object with hash 5abdaaaaae01d117b9d3e54e2e8dfa68fad4e4c8 and elements
#> [1]   6   9   5 103 104 101   7   8 102
#> (in some order)

## Following expressions would return an error if executed:
if(FALSE){
  a[1]
  a[1] <- 44
  a[1:2] <- a[3:4]
}

b <- disord(sample(9))
## Following expressions would also return an error if executed:
if(FALSE){
  a+b  # (not really an example of extraction)
  a[b>5]
  a[b>5] <- 100
  a[b>5] <- a[b>5] + 44
}
```
