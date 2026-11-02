# 'BIDS' subject class

'BIDS' subject class

## Usage

``` r
BIDSSubject(project, subject_code, ..., strict = "raw")

bids_subject(project, subject_code, ..., strict = "raw")
```

## Arguments

- project:

  'BIDS' project instance, see
  [`BIDSProject`](https://dipterix.org/bidsr/reference/bids_project.md),
  or a path to the 'BIDS' project

- subject_code:

  character, subject code with or without the leading `'sub-'`. The
  subject code, after trimming the leading entity key, should not
  contain any additional dash (`'-'`)

- ...:

  passed to the constructor of
  [`BIDSProject`](https://dipterix.org/bidsr/reference/bids_project.md),
  when `project` is a character string

- strict:

  whether to check if the subject folders exist, can be logical or
  characters; when `strict` is character strings, choices can be `'raw'`
  (checking raw-data directory) and/or `'source'` ( for source-data
  directory); `strict=TRUE` is equivalent to checking both; default is
  `'raw'`. There is no checks on derivatives.

## Value

A 'BIDS' subject instance.

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


  subject <- BIDSSubject(project = project, subject_code = "ecog01",
                          strict = FALSE)

  storage_root <- resolve_bids_path(subject, storage = "raw")

  query_bids(subject, "ieeg")

}
```
