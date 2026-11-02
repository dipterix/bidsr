# Create new `bidsr` class definition

By default, all generated classes inherit
[`BIDSClassBase`](https://dipterix.org/bidsr/reference/BIDSClassBase.md),
which provides `S3` generics

## Usage

``` r
new_bids_class(
  name,
  parent = BIDSClassBase,
  abstract = FALSE,
  hidden_names = NULL,
  properties = NULL,
  methods = NULL,
  validator = NULL,
  constructor = NULL
)
```

## Arguments

- name:

  string, required, name of the class

- parent:

  parent class definition, needs to be a `'S7'` class

- abstract:

  whether the class is abstract (`TRUE`) or not (`FALSE`)

- hidden_names:

  vector of string, names of properties and/or methods whose presence
  should be hidden from the users; this will affect `` `$` `` operator,
  or [`names`](https://rdrr.io/r/base/names.html) function. The hidden
  properties or methods cannot be queried via these two ways. However,
  properties can still be accessible via `` `@` `` operator

- properties:

  a named list where the names are the property names that can be
  queried via `` `$` `` or `` `@` `` operators

- methods:

  read-only methods for the class, such as `format` and `print`; if a
  method is a function, then the arguments should start with `self`
  (instance method) or `cls` (class method). In most of the cases,
  changes made to the object will not be carrier out once the the method
  function exits. For changes to the properties, use `setter` functions
  in each property.

- validator:

  validate function; see
  [`new_class`](https://rconsortium.github.io/S7/reference/new_class.html)

- constructor:

  function to custom the constructor; see parameter `'constructor'` at
  [`new_class`](https://rconsortium.github.io/S7/reference/new_class.html)
  for details. Basically A custom constructor should call
  [`S7::new_object()`](https://rconsortium.github.io/S7/reference/new_class.html)
  to create the `'S7'` object. The first argument should be an instance
  of the parent class (if used). The subsequent arguments are used to
  set the properties.

## Value

A `S7` object inheriting the `'bidsr::BIDSClassBase'` class.

## Author

Zhengjia Wang

## Examples

``` r

# ---- Basic usage --------------------------------------------

Range <- new_bids_class(
  "Range",
  properties = list(
    start = bids_property_numeric("start", "required"),
    end = bids_property_numeric("end", "optional")
  ),
  validator = function(self) {
    if(length(self@end) && self@end < self@start) {
      "@end must be great than or equal to @start"
    }
  }
)


r <- Range(start = 10)
r
#> <bidsr::Range>
#>  @ start: num 10
#>  @ end  : int(0) 
# get and set properties with @ or $
r$start
#> [1] 10
r$end <- 40
r$end
#> [1] 40

try(Range(start = c(10, 15), end = 20))
#> Error : <bidsr::Range> object properties are invalid:
#> - @start Must have length 1, but has length 2
try(Range(start = 15, end = 10))
#> Error : <bidsr::Range> object is invalid:
#> - @end must be great than or equal to @start



# ---- hide properties and attributes -------------------------
MyClass <- new_bids_class(
  name = "MyClass",
  properties = list(
    str = bids_property_character(
      name = "str", type = "required"),
    hidden_prop = bids_property_character("hidden_prop")
  ),
  methods = list(
    # read-only methods
    format = function(self, ...) {
      sprintf("MyClass@str -> %s", self$str)
    },
    hidden_method = function(self) {
      "Nonononono"
    }
  ),
  hidden_names = c("hidden_method", "hidden_prop")
)

x <- MyClass(str = "a")
x
#> MyClass@str -> a

# hidden names will not be displayed
names(x)
#> [1] "str"    "format"
as.list(x)
#> $str
#> [1] "a"
#> 

# however, then can still be queried
x$hidden_prop
#> character(0)
x$hidden_method()
#> [1] "Nonononono"



```
