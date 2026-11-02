# Class definitions of 'BIDS' entity

A 'BIDS' entity is an attribute that can be associated with a file,
contributing to the identification of that file as a component of its
file-name in the form of a hyphen-separated key-value pair. The
specification can be found at
<https://bids-specification.readthedocs.io/en/stable/common-principles.html#entities>.

## Usage

``` r
BIDSEntity_label_required(
  key = character(0),
  value = character(0),
  index_format = "%d"
)

BIDSEntity_label_optional(
  key = character(0),
  value = character(0),
  index_format = "%d"
)

BIDSEntity_label_prohibited(
  key = character(0),
  value = character(0),
  index_format = "%d"
)

BIDSEntity_index_required(
  key = character(0),
  value = integer(0),
  index_format = "%d"
)

BIDSEntity_index_optional(
  key = character(0),
  value = integer(0),
  index_format = "%d"
)

BIDSEntity_index_prohibited(
  key = character(0),
  value = integer(0),
  index_format = "%d"
)

BIDSEntity_any_required(
  key = character(0),
  value = character(0),
  index_format = "%d"
)

BIDSEntity_any_optional(
  key = character(0),
  value = character(0),
  index_format = "%d"
)

BIDSEntity_any_prohibited(
  key = character(0),
  value = character(0),
  index_format = "%d"
)
```

## Arguments

- key:

  (string, required) A short string, typically a compression of the
  entity name, which uniquely identifies the entity when part of a
  file-name.

- value:

  A string (label) or a non-negative integer (index); the requisite form
  of the value that gets specified alongside the key whenever the entity
  appears in a file-name. For each entity, the value is of one of two
  possible types:

- index_format:

  for index entities, how to format index values (e.g. padding zeros)
  when formatted as string; default is without padding

  Index:

  :   A non-negative integer, potentially zero-padded for consistent
      width.

  Label:

  :   An alphanumeric string. Note that labels must not collide when
      casing is ignored (`bidsr` does not validate this).

## Value

A 'BIDS' entity object.

## Author

Zhengjia Wang

## Examples

``` r


entity_int <- BIDSEntity_index_optional(key = "run", value = "001")
entity_int$value <- integer()

print(entity_int) # nothing will be printed out
#> 

# subject entity
entity_subject <- BIDSEntity_any_required(key = "sub", value = "HUP225")

print(entity_subject)
#> sub-HUP225

# index
entity_subject$value <- 1

print(entity_subject)
#> sub-1

# format index
entity_subject$index_format <- "%03d"
print(entity_subject)
#> sub-001


# trying to set invalid values will result in errors
try({
  BIDSEntity_index_required(key = "run")
})
#> Error : <bidsr::BIDSEntity_index_required>@value Must have length 1, but has length 0


entity_int <- BIDSEntity_index_required(key = "run", value = "001")

# trying to unset require entity
try({
  entity_int$value <- integer()
})
#> Error : <bidsr::BIDSEntity_index_required>@value Must have length 1, but has length 0

# trying to set invalid entity
try({
  entity_int$value <- "asdad"
})
#> Error in ensure_entity_index(value) : 
#>   Unable to set BIDS entity as index value. Value cannot be converted to integers: ‘asdad’

# trying to set prohibited entiry
try({
  BIDSEntity_index_prohibited("invalid", 123)
})
#> Error : <bidsr::BIDSEntity_index_prohibited>@value Must have length 0, but has length 1

```
