# 'BIDS' project class

'BIDS' project class

## Usage

``` r
BIDSProject(
  path,
  raw_data_relpath = ".",
  source_data_relpath = "sourcedata",
  derivative_data_relpath = "derivatives",
  strict = TRUE
)

bids_project(
  path,
  raw_data_relpath = ".",
  source_data_relpath = "sourcedata",
  derivative_data_relpath = "derivatives",
  strict = TRUE
)
```

## Arguments

- path:

  absolute path to the 'BIDS' project directory;

- raw_data_relpath:

  raw data-set path, relative (to the `path`);

- source_data_relpath:

  source data-set path, relative (to the `path`);

- derivative_data_relpath:

  derivative data-set path, relative (to the `path`);

- strict:

  whether `path` needs to exist; default is `TRUE`

## Value

A 'BIDS' project instance.

## Author

Zhengjia Wang

## Examples

``` r


# Run `download_bids_examples()` first
examples <- download_bids_examples(test = TRUE)
if(!isFALSE(examples)) {

  project_path <- file.path(examples, "ieeg_epilepsy_ecog")

  project <- BIDSProject(
    path = project_path,
    raw_data_relpath = ".",
    derivative_data_relpath = "derivatives"
  )

  project

}


```
