# Download 'BIDS' example data-sets

See <https://github.com/bids-standard/bids-examples> for the full
repository.

## Usage

``` r
download_bids_examples(test = FALSE)
```

## Arguments

- test:

  logical; default is `FALSE`, which downloads the example repository if
  the files are missing; an alternative choice is `TRUE`, which will
  return `FALSE` if the files are missing

## Value

A local path to the example repository exists or when `test=FALSE`; or
simply `FALSE` if the repository is missing and `test=TRUE`.

## Author

Zhengjia Wang

## Examples

``` r

download_bids_examples(test = TRUE)
#> [1] FALSE
```
