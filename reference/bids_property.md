# `'S7'` property for 'BIDS' classes

Used in `property` to generate properties with constraints in class
generators such as
[`new_bids_class`](https://dipterix.org/bidsr/reference/new_bids_class.md).

## Usage

``` r
bids_property(
  name,
  class = S7::class_any,
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = NULL,
  final = FALSE,
  ...
)

bids_property_optional(
  name,
  class = S7::class_any,
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = NULL,
  max_len = 1L,
  ...
)

bids_property_required(
  name,
  class = S7::class_any,
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = NULL,
  len = 1L,
  ...
)

bids_property_prohibited(
  name,
  class = S7::class_any,
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = NULL,
  ...
)

bids_property_recommended(
  name,
  class = S7::class_any,
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = NULL,
  ...,
  max_len = 1L
)

bids_property_deprecated(
  name,
  class = S7::class_any,
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = NULL,
  ...,
  max_len = 1L
)

bids_property_character(
  name,
  type = c("optional", "recommended", "required", "deprecated", "prohibited"),
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = NULL,
  ...,
  class = S7::class_character
)

bids_property_collapsed_character(
  name,
  type = c("optional", "recommended", "required", "deprecated", "prohibited"),
  collapse = " ",
  ...,
  class = S7::class_character
)

bids_property_choice(
  name,
  choices,
  type = c("optional", "recommended", "required", "deprecated", "prohibited"),
  ...,
  class = S7::class_character
)

bids_property_numeric(
  name,
  type = c("optional", "recommended", "required", "deprecated", "prohibited"),
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = NULL,
  ...,
  class = S7::class_numeric
)

bids_property_integerish(
  name,
  type = c("optional", "recommended", "required", "deprecated", "prohibited"),
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = NULL,
  ...,
  class = S7::class_numeric
)

bids_property_list(
  name,
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = NULL,
  ...,
  class = S7::class_list
)

bids_property_named_list(
  name,
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = list(),
  ...,
  class = S7::class_list
)

bids_property_unnamed_list(
  name,
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = NULL,
  ...,
  class = S7::class_list
)

bids_property_entity_list(
  name,
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = list(),
  ...,
  class = S7::class_list,
  identifier = NULL,
  schema_key = NA,
  bids_version = current_bids_version()
)

bids_property_tabular_column_descriptor_list(
  name,
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = list(),
  ...,
  class = S7::class_list
)

bids_property_data_frame(
  name,
  getter = NULL,
  setter = NULL,
  validator = NULL,
  default = NULL,
  ...,
  class = S7::class_data.frame
)

bids_property_tabular_content(
  name = "content",
  setter = NULL,
  ...,
  name_meta = "meta",
  lower_case_column_names = FALSE
)

bids_property_tabular_meta(
  name = "meta",
  setter = NULL,
  preset = NULL,
  ...,
  name_content = "content"
)
```

## Arguments

- name:

  required, string, name of the property

- class:

  `'S7'` class

- getter, setter, validator, default:

  see
  [`new_property`](https://rconsortium.github.io/S7/reference/new_property.html)

- final:

  whether the property is final once initialized; default is false; this
  is for properties that should not be altered

- ...:

  passed to other methods

- max_len:

  for `type='optional'`, maximum vector length of the property; default
  is 1

- len:

  for `type='required'`, vector length of the property; default is 1

- type:

  type of the property, can be `'required'`, `'optional'`, or
  `'prohibited'`

- collapse:

  for collapsed property, passed to
  [`paste`](https://rdrr.io/r/base/paste.html)

- choices:

  for properties that can only be chosen from given choices; a character
  strings of candidate choices.

- identifier:

  `"data_type/suffix"` combination to get entity rules

- schema_key:

  'BIDS' schema key if explicit entity rules is needed

- bids_version:

  'BIDS' version to query the entity rules

- name_meta:

  for tabular content, the name of the meta property; default is
  `"meta"`

- lower_case_column_names:

  for tabular content, whether to convert column names to lower case;
  default is `FALSE`

- preset:

  a list of preset meta data; default is `NULL`

- name_content:

  for tabular meta, the name of the content property; default is
  `"content"`

## Value

All functions call
[`new_property`](https://rconsortium.github.io/S7/reference/new_property.html)
internally.

## Author

Zhengjia Wang

## Examples

``` r


MyClass <- new_bids_class(
  name = "MyClass",
  properties = list(
    str = bids_property_character(
      name = "str",
      type = "required",
      validator = function(value) {
        if (length(value) == 1 &&
            !isTRUE(is.na(value)) && nzchar(value)) {
          return()
        }
        return(sprintf("Invalid `str`: %s", paste(sQuote(value), collapse = ", ")))
      }
    )
  ),
  methods = list(
    # read-only methods
    format = function(self, ...) {
      sprintf("MyClass@str -> %s", self$str)
    }
  )
)

instance <- MyClass(str = "aha")
instance
#> MyClass@str -> aha

instance$str <- "111"
instance
#> MyClass@str -> 111


# what if you enter illegal values

try({
  MyClass(str = "")
})
#> Error : <bidsr::MyClass> object properties are invalid:
#> - @str Invalid `str`: ‘’

try({
  MyClass(str = NA_character_)
})
#> Error : <bidsr::MyClass> object properties are invalid:
#> - @str Contains missing values (element 1)

try({
  MyClass(str = 1)
})
#> Error : <bidsr::MyClass> object properties are invalid:
#> - @str must be <character>, not <double>


```
