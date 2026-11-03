# 'BIDS' samples table class

A tabular containing a list of samples and their metadata. The class is
a child class of
[`BIDSTabular`](https://dipterix.org/bidsr/reference/BIDSTabular.md),
hence see the methods there. The original specification is at
<https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#samples-file>.

## Usage

``` r
get_bids_samples(x, ...)

BIDSTabularSamples(content, meta = NULL)
```

## Arguments

- x:

  R object such as file path, project instances, etc.

- ...:

  passed to other methods or ignored

- content, meta:

  see
  [`BIDSTabular`](https://dipterix.org/bidsr/reference/BIDSTabular.md)

## Value

A `BIDSTabularSamples` instance inheriting
[`BIDSTabular`](https://dipterix.org/bidsr/reference/BIDSTabular.md).

## Author

Zhengjia Wang

## Examples

``` r


# basic
tabular <- BIDSTabularSamples(
  data.frame(
    sample_id = "sample-001",
    participant_id = "sub-001",
    sample_type = "cell line"
  )
)
tabular
#> <BIDS Tabular>[BIDSTabular_samples]
#> $meta:
#> {
#>   "sample_id": {
#>     "LongName": "Sample ID",
#>     "Description": "[Required, string] A sample identifier of the form `sample-<label>`, matching a sample entity found in the dataset",
#>     "TermURL": "https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#samples-file"
#>   },
#>   "participant_id": {
#>     "LongName": "Participant ID",
#>     "Description": "[Required, string] A participant identifier of the form `sub-<label>`, matching a participant entity found in the dataset",
#>     "TermURL": "https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#participants-file"
#>   },
#>   "sample_type": {
#>     "LongName": "Sample Type",
#>     "Description": "[Required, string] Biosample type defined by ENCODE Biosample Type",
#>     "Levels": {
#>       "cell line": "cell line",
#>       "in vitro differentiated cells": "in vitro differentiated cells",
#>       "primary cell": "primary cell",
#>       "cell-free sample": "cell-free sample",
#>       "cloning host": "cloning host",
#>       "tissue": "tissue",
#>       "whole organisms": "whole organisms",
#>       "organoid": "organoid",
#>       "technical sample": "technical sample"
#>     },
#>     "TermURL": "https://www.encodeproject.org/profiles/biosample_type/"
#>   },
#>   "pathology": {
#>     "LongName": "Pathology",
#>     "Description": "[Recommended, string] String value describing the pathology of the sample or type of control. When different from healthy, pathology SHOULD be specified. The pathology may be specified in either `samples.tsv` or `sessions.tsv`, depending on whether the pathology changes over time."
#>   },
#>   "derived_from": {
#>     "LongName": "Derived From",
#>     "Description": "[Recommended, string] `sample-<label>` entity from which a sample is derived, for example a slice of tissue (`sample-02`) derived from a block of tissue (`sample-01`)."
#>   }
#> }
#> 
#> $content:
#>     sample_id participant_id sample_type
#>        <char>         <char>      <char>
#> 1: sample-001        sub-001   cell line


# convert existing tabular
tabular <- BIDSTabular(
  data.frame(
    sample_id = "sample-001",
    participant_id = "sub-001",
    sample_type = "cell line"
  )
)
tabular <- as_bids_tabular(tabular, cls = BIDSTabularSamples)


# save to tsv
tsv <- file.path(tempdir(), "samples.tsv")
paths <- save_bids_tabular(tabular, tsv)
print(paths)
#> $table_path
#> /tmp/Rtmp9cc0w1/samples.tsv
#> 
#> $sidecar_path
#> /tmp/Rtmp9cc0w1/samples.json
#> 

# use base R to read
read.table(tsv, header = TRUE, na.strings = "n/a")
#>    sample_id participant_id sample_type
#> 1 sample-001        sub-001   cell line

# get sidecar
cat(readLines(paths$sidecar_path), sep = "\n")
#> {}

# clean up
unlink(tsv)
unlink(paths$sidecar_path)

```
