# Class definitions and utilities for 'BIDS' tabular

Official specification link:
<https://bids-specification.readthedocs.io/en/stable/common-principles.html#tabular-files>.
Function `save_tabular` is the high-level generic function that by
default calls low-level function `save_bids_tabular_default` by default.

## Usage

``` r
as_bids_tabular(x, ...)

save_bids_tabular(x, path, meta = TRUE, ...)

BIDSTabularColumnDescriptor(..., .list = list())

BIDSTabularMetaSidecar(columns = list())

BIDSTabular(content, meta = NULL)

save_bids_tabular_default(
  x,
  path,
  meta = TRUE,
  compact_meta = TRUE,
  milliseconds = TRUE,
  utc = TRUE,
  ...
)

new_bids_tabular_class(
  table_name,
  parent = BIDSTabular,
  content_setter = NULL,
  meta_preset = NULL,
  prepare_save = NULL,
  lower_case_column_names = FALSE
)
```

## Arguments

- x:

  R object that can be converted (e.g. list, table), or a path to a
  tabular file.

- ..., .list:

  for `BIDSTabularColumnDescriptor`, this is a list of key-value
  properties; for `as_bids_tabular`, this is passed to
  `BIDSTabularMetaSidecar`

- path:

  path to save the file; the file is always saved as tabular-separated
  value ('TSV') format

- meta:

  instance of `BIDSTabularMetaSidecar`, a class containing a list of
  descriptors for each column (see argument `columns`)

- columns:

  a named list, where each key correspond to a table column name, and
  each item is a named list of descriptors, or a
  `BIDSTabularColumnDescriptor` instance

- content:

  a data frame or table with column names non-blanks and possibly all in
  snake-cases (see specification); `bidsr` does not check on the column
  names for compatibility concerns. However users should respect the
  specification and use the recommended conventions

- compact_meta:

  logical, whether the meta side-car ('JSON' file) should use compact
  format; default is true

- milliseconds, utc:

  used to convert
  [`nanotime`](https://eddelbuettel.github.io/nanotime/man/nanotime.html)
  to 'BIDS' time-stamp format; default is to keep the milliseconds and
  use 'UTC' timezone.

- table_name:

  name of the table, used to generate a new class; the class name will
  be `BIDSTabular_<table_name>`

- parent:

  parent class of the new class; default is `BIDSTabular`

- content_setter:

  a `setter` function to set the content; see
  [`bids_property`](https://dipterix.org/bidsr/reference/bids_property.md)

- meta_preset:

  a `preset` function to set the meta; see `BIDSTabularMetaSidecar`

- prepare_save:

  a function to prepare the content before saving; should take the
  `BIDSTabular` object as the first argument, and return the content to
  be saved

- lower_case_column_names:

  if `TRUE`, the column names will be converted to lower case; default
  is `TRUE`

## Value

A component in `BIDSTabular`.

## Author

Zhengjia Wang

## Examples

``` r



# convert a data table into BIDS tabular
table <- data.frame(
  a = c(1, 2, 3, NA, NA, 6, 7, 8, 9, 10),
  b = sample(c('a', 'b'), size = 10, replace = TRUE)
)

# basic
as_bids_tabular(table)
#> <BIDS Tabular>[BIDSTabular]
#> $meta:
#> {}
#> 
#> $content:
#>         a      b
#>     <num> <char>
#>  1:     1      a
#>  2:     2      a
#>  3:     3      b
#>  4:    NA      a
#>  5:    NA      a
#>  6:     6      b
#>  7:     7      a
#>  8:     8      a
#>  9:     9      a
#> 10:    10      b

# add descriptors
tabular <- as_bids_tabular(
  table,
  a = list(LongName = "An integer"),
  b = list("Levels" = list('a' = "Abnormal", 'b' = "Bipolar"))
)
tabular
#> <BIDS Tabular>[BIDSTabular]
#> $meta:
#> {
#>   "a": {
#>     "LongName": "An integer"
#>   },
#>   "b": {
#>     "Levels": {
#>       "a": "Abnormal",
#>       "b": "Bipolar"
#>     }
#>   }
#> }
#> 
#> $content:
#>         a      b
#>     <num> <char>
#>  1:     1      a
#>  2:     2      a
#>  3:     3      b
#>  4:    NA      a
#>  5:    NA      a
#>  6:     6      b
#>  7:     7      a
#>  8:     8      a
#>  9:     9      a
#> 10:    10      b


# query data
is.data.frame(tabular$content)
#> [1] TRUE
tabular$content$a
#>  [1]  1  2  3 NA NA  6  7  8  9 10

# query meta
tabular$meta$columns$a
#> {
#>   "LongName": "An integer"
#> }

# save to tsv
tsv <- tempfile(fileext = ".tsv")
paths <- save_bids_tabular(tabular, tsv)
print(paths)
#> $table_path
#> /tmp/RtmpE4U7WX/file18f240bc94aa.tsv
#> 
#> $sidecar_path
#> /tmp/RtmpE4U7WX/file18f240bc94aa.json
#> 

# use base R to read
read.table(tsv, header = TRUE, na.strings = "n/a")
#>     a b
#> 1   1 a
#> 2   2 a
#> 3   3 b
#> 4  NA a
#> 5  NA a
#> 6   6 b
#> 7   7 a
#> 8   8 a
#> 9   9 a
#> 10 10 b

# get sidecar
cat(readLines(paths$sidecar_path), sep = "\n")
#> {
#>   "a": {
#>     "LongName": "An integer"
#>   },
#>   "b": {
#>     "Levels": {
#>       "a": "Abnormal",
#>       "b": "Bipolar"
#>     }
#>   }
#> }

unlink(tsv)
unlink(paths$sidecar_path)


```
