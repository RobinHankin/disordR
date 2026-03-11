# The \`disordR\` package: design philosophy and a use-case in multivariate polynomials

![](../../../_temp/Library/disordR/help/figures/disordR.png)

#### Abstract

To cite the `disordR` package in publications please use Hankin (2022).
This document motivates the concept of “disordered vector” using
coefficients of multivariate polynomials as represented by associative
maps such as the `STL` map class as used by the `mvp` package. Values
and keys of a map are stored in an implementation-specific way so
certain extraction and replacement operations should be forbidden. For
example, consider a finite set `S` of real numbers. The “first” element
of `S` is implementation specific (because a set does not specify the
order of its elements)…but the maximum value of `S` has a well-defined
value, as does the sum. The `disordR` package makes it impossible to
execute forbidden operations (such as finding the “first” element),
while allowing transparent R idiom for permitted operations such as
finding the maximum or sum. Note that one cannot dispense with order
entirely, as we wish to consider keys and values of `STL` objects
separately, but we need to retain the ability to match individual keys
with their corresponding values.

An illustrative R session is given in which the `disordR` package is
used abstractly, without reference to any particular application, and
then shows how the `disordR` package is used in the `mvp` package.
`disordR` is used in the `clifford`, `freealg`, `hyper2`, `mvp`,
`spray`, `stokes`, and `weyl` packages.

#### Introduction

In `C++` (ISO Central Secretary 1998), the `STL map` class (Josuttis
1999) is an object that associates a value to each of a set of keys.
Accessing values or keys of a `map` object is problematic because the
value-key pairs are not stored in a well-defined order. The situation is
applicable to any package which uses the `map` objects. Consider, for
example, the `mvp` package which deals with multinomials using `STL`
maps. An `mvp` object is a map from terms to coefficients, and a map has
no intrinsic ordering: the maps

    x -> 1, y -> 3, xy -> 3, xy^3 -> 4

and

    xy^3 -> 4, xy -> 3, x -> 1, y -> 3

are the same map and correspond to the same multinomial (symbolically,
$x + 3y + 3xy + 4xy^{3} = 4xy^{3} + 3xy + x + 3y$). Thus the
coefficients of the multinomial might be `c(1,3,3,4)` or `c(4,3,1,3)`,
or indeed any ordering. Internally, the elements are stored in some
order but the order used is implementation-specific. Quite often, I am
interested in the coefficients *per se*, without consideration of their
meaning in the context of a multivariate polynomial. I might ask:

- “How many coefficients are there?”
- “What is the largest coefficient?”
- “Are any coefficients exactly equal to one?”
- “How many coefficients are greater than 2?”

These are reasonable and mathematically meaningful questions because
they have an answer that is independent of the order in which the
coefficients are stored. Compare a meaningless question: “what is the
second coefficient?”. This is meaningless because of the order ambiguity
discussed above: the answer is at best implementation-specific, but
fundamentally it is a question that one should not be allowed to ask.

To deal with the coefficients in isolation in R, one might be tempted to
use a multiset. However, this approach does not allow one to link the
coefficients with the terms. Suppose I coerce the coefficients to a
multiset object (as per the `sets` package, for example): then it is
impossible to extract the terms with coefficient greater than 2 (which
would be the polynomial $3y + 3xy + 4xy^{3}$) because the link between
the coefficients and the terms is not included in the multiset object.
Sensible questions involving this aspect of `mvp` objects might be:

- Give me all terms with coefficients greater than 2
- Give me all terms with positive coefficients
- Give me all terms with integer coefficients

and these questions cannot be answered if the the coefficients are
stored as a multiset (compare inadmissible questions such as “give me
the first three terms”). Further note that replacement methods are
mathematically meaningful, for example:

- Set any term with a negative coefficient to zero
- Add 100 to any coefficient less than 30

Again these operations are reasonable but precluded by multiset
formalism (compare inadmissible replacements: “replace the first two
terms with zero”, or “double the last term” would be inadmissible).

***What we need is a system that forbids stupid questions and stupid
operations, while permitting sensible questions and operations***

The `disord` class of the `disordR` package is specificially designed
for this situation. This class of object has a slot for the coefficients
in the form of a numeric R vector, but also another slot which uses hash
codes to prevent users from misusing the ordering of the numeric vector.

For example, a multinomial `x+2y+3z` might have coefficients `c(1,2,3)`
or `c(3,1,2)`. Package idiom to extract the coefficients of a
multivariate polynomial `a` is `coeffs(a)`; but this cannot return a
standard numeric vector. If stored as a numeric vector, the user might
ask “what is the first element?” and this question should not be asked
\[and certainly not answered!\], because the elements are stored in an
implementation-specific order. The `disordR` package uses `disord`
objects which are designed to return an error if such inadmissible
questions are asked. But `disord` objects can answer admissible
questions and perform admissible operations.

Suppose we have two multivariate polynomials, `a` as defined as above
with `a=x+2y+3z` and `b=x+3y+4z`. Even though the sum `a+b` is
well-defined algebraically, idiom such as `coeffs(a) + coeffs(b)` is not
defined because there is no guarantee that the coefficients of the two
multivariate polynomials are stored in the same order. We might have
`c(1,2,3)+c(1,3,4)=c(2,5,7)` or `c(1,2,3)+c(1,4,3)=c(2,6,6)`, with
neither being more “correct” than the other. In the package, this
ambiguity is rendered void: `coeffs(a) + coeffs(b)` will return an
error. Note carefully that `a+b`—and hence `coeffs(a+b)`—is perfectly
well defined, although the result is subject to the same ambiguity as
`coeffs(a)`.

In the same way, `coeffs(a) + 1:3` is not defined and will return an
error. Further, idiom such as `coeffs(a) <- 1:3`and
`coeffs(a) <- coeffs(b)` are not defined and will also return an error.
However, note that

    coeffs(a) + coeffs(a)
    coeffs(a) + coeffs(a)^2
    coeffs(a) <- coeffs(a)^2
    coeffs(a) <- coeffs(a)^2 + 7

are perfectly well defined, with package idiom behaving as expected.

Idiom such as `disord(a) <- disord(a)^2` is OK: one does not need to
know the order of the coefficients on either side, so long as the order
is the same on both sides. The idiomatic English equivalent would be:
“the coefficient of each term of `a` becomes its square”; note that this
operation is insensitive to the order of coefficients. The whole shebang
is intended to make idiom such as `coeffs(a) <- coeffs(a)%%2` possible,
so we can manipulate polynomials over finite rings, here $Z/2Z$.

The replacement methods are defined so that an expression like
`coeffs(a)[coeffs(a) < 5] <- 0` works as expected; the English idiom
would be “replace any coefficient less than 5 with 0”.

To fix ideas, consider a fixed small mvp object:

``` r
library("mvp")
```

    ## 
    ## Attaching package: 'mvp'

    ## The following object is masked from 'package:base':
    ## 
    ##     trunc

``` r
a <- as.mvp("5 a c^3 + a^2 d^2 f^2 + 4 a^3 b e^3 + 3 b c f + 2 b^2 e^3")
a
```

    ## mvp object algebraically equal to
    ## 5 a c^3 + a^2 d^2 f^2 + 4 a^3 b e^3 + 3 b c f + 2 b^2 e^3

Extraction presents issues; consider `coeffs(a)<3`. This object has
Boolean elements but has the same ordering ambiguity as `coeffs(a)`. One
might expect that we could use this to extract elements of `coeffs(a)`:
specifically, those elements less than 5. We may use replace methods for
coefficients if this makes sense. Idiom such as

    coeffs(a)[coeffs(a)<5] <- 4 + coeffs(a)[coeffs(a)<5]
    coeffs(a) <- pmax(a,3)

is algebraically meaningful and allowed in the package. Idiomatically:
“Add 4 to any element less than 5”; “coefficients become the parallel
maximum of themselves and 3” respectively. Further note that
`coeffs(a) <- rev(coeffs(a))` is disallowed (although
`coeffs(a) <- rev(rev(coeffs(a)))` is meaningful and admissible).

So the output of `coeffs(x)` is defined only up to an unknown
rearrangement. The same considerations apply to the output of
[`vars()`](https://robinhankin.github.io/mvp/reference/coeffs.html),
which returns a list of character vectors in an undefined order, and the
output of
[`powers()`](https://robinhankin.github.io/mvp/reference/coeffs.html),
which returns a numeric list whose elements are in an undefined order.
However, even though the order of these three objects is undefined
individually, their ordering is jointly consistent in the sense that the
first element of `coeffs(x)` corresponds to the first element of
`vars(x)` and the first element of `powers(x)`. The identity of this
element is not defined—but whatever it is, the first element of all
three accessor methods refers to it.

Note also that a single term (something like `4a3*b*c^6`) has the same
issue: the variables are not stored in a well-defined order. This does
not matter because the algebraic value of the term does not depend on
the order in which the variables appear and this term would be
equivalent to `4bc^6*a^3`.

#### An R session with the `disordR` package

We will use the `disordR` package to show how the idiom works.

``` r
library("disordR")
set.seed(0)
a <- rdis()
a
```

    ## A disord object with hash bbf425ee5328cfd9946db11fba9f29be329e5d6c and elements
    ## [1] 9 4 7 1 2 7 2 3 1
    ## (in some order)

Object `a` is a `disord` object but it behaves similarly to a regular
numeric vector in many ways:

``` r
a^2
```

    ## A disord object with hash bbf425ee5328cfd9946db11fba9f29be329e5d6c and elements
    ## [1] 81 16 49  1  4 49  4  9  1
    ## (in some order)

``` r
a+1/a
```

    ## A disord object with hash bbf425ee5328cfd9946db11fba9f29be329e5d6c and elements
    ## [1] 9.111111 4.250000 7.142857 2.000000 2.500000 7.142857 2.500000 3.333333
    ## [9] 2.000000
    ## (in some order)

Above, note how the result has the same hash code as `a`. Other
operations that make sense are
[`max()`](https://rdrr.io/r/base/Extremes.html) and
[`sort()`](https://robinhankin.github.io/disordR/reference/misc.md):

``` r
max(a)
```

    ## [1] 9

``` r
sort(a)
```

    ## [1] 1 1 2 2 3 4 7 7 9

Above, see how the result is a standard numeric vector. However,
inadmissible operations give an error:

``` r
a[1]  # asking for the first element is inadmissible
```

    ## Error in `.local()`:
    ## ! if using a regular index to extract, must extract each element once and once only (or none of them)

``` r
a[1] <- 1000 # also cannot replace the first element
```

    ## Error in `.local()`:
    ## ! if using a regular index to replace, must specify each element once and once only

Standard R semantics generally work as expected:

``` r
x <- a + 1/a
x
```

    ## A disord object with hash bbf425ee5328cfd9946db11fba9f29be329e5d6c and elements
    ## [1] 9.111111 4.250000 7.142857 2.000000 2.500000 7.142857 2.500000 3.333333
    ## [9] 2.000000
    ## (in some order)

``` r
y <- a*2-9
y
```

    ## A disord object with hash bbf425ee5328cfd9946db11fba9f29be329e5d6c and elements
    ## [1]  9 -1  5 -7 -5  5 -5 -3 -7
    ## (in some order)

``` r
x+y
```

    ## A disord object with hash bbf425ee5328cfd9946db11fba9f29be329e5d6c and elements
    ## [1] 18.1111111  3.2500000 12.1428571 -5.0000000 -2.5000000 12.1428571 -2.5000000
    ## [8]  0.3333333 -5.0000000
    ## (in some order)

Above, observe that objects `a`, `x` and `y` have the same hash code:
they are “compatible”, in `disordR` idiom. However, if we try to combine
object `a` with another object with different hash, we get errors:

``` r
b <- rdis()
b
```

    ## A disord object with hash b302aa6faf214f3683aa64360a0f39f3ac2bd64a and elements
    ## [1] 5 6 7 9 5 5 9 9 5
    ## (in some order)

``` r
a
```

    ## A disord object with hash bbf425ee5328cfd9946db11fba9f29be329e5d6c and elements
    ## [1] 9 4 7 1 2 7 2 3 1
    ## (in some order)

``` r
a+b
```

    ## 
    ## disordR discipline error in:

    ## a + b

    ## Error in `check_matching_hash()`:
    ## ! 
    ## hash codes bbf425ee5328cfd9946db11fba9f29be329e5d6c and b302aa6faf214f3683aa64360a0f39f3ac2bd64a do not match

The error is given because objects `a` and `b` are stored in an
implementation-specific order (we say that `a` and `b` are
*incompatible*). In the package, many extract and replace methods are
implemented whenever this is admissible:

``` r
a[a<0.5] <- 0  # round down
a
```

    ## A disord object with hash bbf425ee5328cfd9946db11fba9f29be329e5d6c and elements
    ## [1] 9 4 7 1 2 7 2 3 1
    ## (in some order)

``` r
b[b>0.6] <- b[b>0.6] + 3  # add 3 to every element greater than 0.6
b
```

    ## A disord object with hash b302aa6faf214f3683aa64360a0f39f3ac2bd64a and elements
    ## [1]  8  9 10 12  8  8 12 12  8
    ## (in some order)

Usual semantics follow, provided one is careful to maintain the hash
code:

``` r
d <- disord(1:10)
d
```

    ## A disord object with hash f4c26816ed01c4e73464f14e38051ee97cb89ebd and elements
    ##  [1]  1  2  3  4  5  6  7  8  9 10
    ## (in some order)

``` r
e <- 10 + 3*d - d^2
e
```

    ## A disord object with hash f4c26816ed01c4e73464f14e38051ee97cb89ebd and elements
    ##  [1]  12  12  10   6   0  -8 -18 -30 -44 -60
    ## (in some order)

``` r
e<4
```

    ## A disord object with hash f4c26816ed01c4e73464f14e38051ee97cb89ebd and elements
    ##  [1] FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## (in some order)

``` r
d[e<4] <- e[e<4]
d
```

    ## A disord object with hash f4c26816ed01c4e73464f14e38051ee97cb89ebd and elements
    ##  [1]   1   2   3   4   0  -8 -18 -30 -44 -60
    ## (in some order)

Above, the replacement command works because `d` and `e` *and* `e<4`
\[which is a Boolean `disord` object\] all have the same hash code.

## An R session with the `mvp` package

The `mvp` package implements multivariate polynomials using the `STL`
map class. Following commands only work as intended here with
`mvp >= 1.0-12`. Below we see how `disordR` idiom allows mathematically
meaningful operation while suppressing inadmissible ones:

``` r
library("mvp")
set.seed(0)
a <- rmvp()
b <- rmvp()
a
```

    ## mvp object algebraically equal to
    ## 3 + 6 a + 3 a b e f^2 + 4 a d^2 e f^2 + 5 a^3 b^2 c + 4 b c^2 d e^2 + 6 b^3 d e

``` r
b
```

    ## mvp object algebraically equal to
    ## 4 + 5 a c + a c d^2 + 4 a^3 e f + 3 b e^2 f + 3 b f + 3 c f^4

Observe that standard multivariate polynomial algebra works:

``` r
a + 2*b
```

    ## mvp object algebraically equal to
    ## 11 + 6 a + 3 a b e f^2 + 10 a c + 2 a c d^2 + 4 a d^2 e f^2 + 5 a^3 b^2 c + 8
    ## a^3 e f + 4 b c^2 d e^2 + 6 b e^2 f + 6 b f + 6 b^3 d e + 6 c f^4

``` r
(a+b)*(a-b) == a^2-b^2   # should be TRUE (expression is quite long)
```

    ## [1] TRUE

We can extract the coefficients of these polynomials using the
[`coeffs()`](https://robinhankin.github.io/mvp/reference/coeffs.html)
function:

``` r
coeffs(a)
```

    ## A disord object with hash 28956179ca523d4271d4442cae8a1066438ecd89 and elements
    ## [1] 3 6 3 4 5 4 6
    ## (in some order)

``` r
coeffs(b)
```

    ## A disord object with hash c54f16fc8e819253a6fb82abff288cbe36b76aec and elements
    ## [1] 4 5 1 4 3 3 3
    ## (in some order)

observe that the coefficients are returned as a `disord` object. We may
manipulate the coefficients of a polynomial in many ways. We may do the
following things:

``` r
coeffs(a)[coeffs(a) < 4] <- 0   # set any coefficient of a that is <4 to zero
a
```

    ## mvp object algebraically equal to
    ## 6 a + 4 a d^2 e f^2 + 5 a^3 b^2 c + 4 b c^2 d e^2 + 6 b^3 d e

``` r
coeffs(b) <- coeffs(b)%%2       # consider coefficients of b modulo 2
b
```

    ## mvp object algebraically equal to
    ## a c + a c d^2 + b e^2 f + b f + c f^4

However, many operations which have reasonable idiom are in fact
meaningless and are implicitly prohibited. For example:

``` r
x <- rmvp()     # set up new mvp objects x and y
y <- rmvp()
```

Then the following should all produce errors:

``` r
coeffs(x) + coeffs(y)  # order implementation specific
```

    ## 
    ## disordR discipline error in:

    ## coeffs(x) + coeffs(y)

    ## Error in `check_matching_hash()`:
    ## ! 
    ## hash codes b50be1f42b64dd2676fd8ac1a0a1cb2947cfe0c1 and 1a282859aaadfaa35657bce17433b7eff4e5753d do not match

``` r
coeffs(x) <- coeffs(y) # ditto
```

    ## Error in `coeffs<-.mvp`:
    ## ! consistent(vars(x), value) is not TRUE

``` r
coeffs(x) <- 1:2       # replacement value not length 1
```

    ## 
    ## disordR discipline error in:

    ## .local(x = x, i = i, j = j, value = value)

    ## Error in `check_matching_hash()`:
    ## ! 
    ## cannot combine disord object with hash code b50be1f42b64dd2676fd8ac1a0a1cb2947cfe0c1 with a vector

``` r
coeffs(x)[coeffs(x) < 3] <- coeffs(x)[coeffs(y) < 3]
```

    ## 
    ## disordR discipline error in:

    ## .local(x = x, i = i, j = j, drop = drop)

    ## Error in `check_matching_hash()`:
    ## ! 
    ## hash codes b50be1f42b64dd2676fd8ac1a0a1cb2947cfe0c1 and 1a282859aaadfaa35657bce17433b7eff4e5753d do not match

### `vars()` and `powers()` return `disord` objects

The
[`disord()`](https://robinhankin.github.io/disordR/reference/disord.html)
function takes a list argument, and this is useful for working with
`mvp` objects:

``` r
(a <- as.mvp("x^2 + 4 - 3*x*y*z"))
```

    ## mvp object algebraically equal to
    ## 4 - 3 x y z + x^2

``` r
vars(a)
```

    ## A disord object with hash e09bcadc2a1cd06b9c3239445d73ec684acf17e6 and elements
    ## [[1]]
    ## character(0)
    ## 
    ## [[2]]
    ## [1] "x" "y" "z"
    ## 
    ## [[3]]
    ## [1] "x"
    ## 
    ## (in some order)

``` r
powers(a)
```

    ## A disord object with hash e09bcadc2a1cd06b9c3239445d73ec684acf17e6 and elements
    ## [[1]]
    ## integer(0)
    ## 
    ## [[2]]
    ## [1] 1 1 1
    ## 
    ## [[3]]
    ## [1] 2
    ## 
    ## (in some order)

``` r
coeffs(a)
```

    ## A disord object with hash e09bcadc2a1cd06b9c3239445d73ec684acf17e6 and elements
    ## [1]  4 -3  1
    ## (in some order)

Note that the hash of all three objects is identical, generated from the
polynomial itself (not just the relevant element of the three-element
list that is an `mvp` object). This allows us to do some rather
interesting things:

``` r
double <- function(x){2*x}
(a <- rmvp())
```

    ## mvp object algebraically equal to
    ## 4 + 2 a b d f + 5 a b e + 7 a f + 4 a^2 f^2 + d e^2 f^3 + e

``` r
pa <- powers(a)
va <- vars(a)
ca <- coeffs(a)
pa[ca<4] <- sapply(pa,double)[ca<4]
mvp(va,pa,ca)
```

    ## mvp object algebraically equal to
    ## 4 + 5 a b e + 7 a f + 2 a^2 b^2 d^2 f^2 + 4 a^2 f^2 + d^2 e^4 f^6 + e^2

Above, `a` was a multivariate polynomial and we doubled the powers of
all variables in terms with coefficients less than 4. Or even:

``` r
a <- as.mvp("3 + 5*a*b - 7*a*b*x^2 + 2*a*b^2*c*d*x*y -6*x*y + 8*a*b*c*d*x")
a
```

    ## mvp object algebraically equal to
    ## 3 + 5 a b + 8 a b c d x - 7 a b x^2 + 2 a b^2 c d x y - 6 x y

``` r
pa <- powers(a)
va <- vars(a)
ca <- coeffs(a)
va[sapply(pa,length) > 4] <- sapply(va,toupper)[sapply(pa,length) > 4]
mvp(va,pa,ca)
```

    ## mvp object algebraically equal to
    ## 3 + 8 A B C D X + 2 A B^2 C D X Y + 5 a b - 7 a b x^2 - 6 x y

Above, we took multivariate polynomial `a` and replaced the variable
names in every term with more than four variables with their uppercase
equivalents.

## References

Hankin, Robin K. S. 2022. “Disordered Vectors in R: Introducing the
disordR Package.” arXiv. <https://doi.org/10.48550/ARXIV.2210.03856>.

ISO Central Secretary. 1998. “Programming Languages—C++. ISO/IEC
144882.” First. American National Standard Institute.

Josuttis, N. M. 1999. *The c++ Standard Library: A Tutorial and
Reference*. Addison-Wesley.
