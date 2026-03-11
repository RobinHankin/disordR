# Class `"disord"`

The `disord` class provides basic arithmetic and extract/replace methods
for disord objects.

## Objects from the Class

Objects can be created by calls of the form `new("disord", ...)`,
although functions `disord()` and (eventually) `as.disord()` are more
user-friendly.

## Slots

- `.Data`::

  Object of class `vector` that specifies the elements

- `hash`::

  Object of class `character` that specifies the hash code

## Author

Robin K. S. Hankin

## Examples

``` r
showClass("disord")
#> Class "disord" [package "disordR"]
#> 
#> Slots:
#>                           
#> Name:      .Data      hash
#> Class:    vector character
#> 
#> Extends: 
#> Class "vector", from data part
```
