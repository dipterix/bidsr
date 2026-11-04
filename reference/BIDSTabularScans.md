# 'BIDS' scans table class

A tabular containing a list of scans and their metadata. The class is a
child class of
[`BIDSTabular`](https://dipterix.org/bidsr/reference/BIDSTabular.md),
hence see the methods there. The original specification is at
<https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#scans-file>.

## Usage

``` r
BIDSTabularScans(content, meta = NULL)
```

## Arguments

- content, meta:

  see
  [`BIDSTabular`](https://dipterix.org/bidsr/reference/BIDSTabular.md)

## Value

A `BIDSTabularScans` instance inheriting
[`BIDSTabular`](https://dipterix.org/bidsr/reference/BIDSTabular.md).

## Author

Zhengjia Wang

## Examples

``` r


# basic
tabular <- BIDSTabularScans(
  data.frame(
    filename = c(
      "func/sub-control01_task-nback_bold.nii.gz",
      "func/sub-control01_task-motor_bold.nii.gz",
      "meg/sub-control01_task-rest_split-01_meg.nii.gz"
    ),
    acq_time = c(
      "1877-06-15T13:45:30",
      "1877-06-15T13:55:33",
      "1877-06-15T12:15:27"
    )
  )
)

# No ending Z, time is interpreted as local time
# tabular uses UTC time
tabular
#> <BIDS Tabular>[BIDSTabular_scans]
#> $meta:
#> {
#>   "filename": {
#>     "LongName": "Filename",
#>     "Description": "[Required, string] Relative paths to files. There MUST be exactly one row for each file. Values in filename MUST be unique. This column must appear first in the file."
#>   },
#>   "acq_time": {
#>     "LongName": "Acquisition Time",
#>     "Description": "[Optional, string] Acquisition time refers to when the first data point in each run was acquired. Furthermore, if this header is provided, the acquisition times of all files from the same recording MUST be identical. Datetime format and their deidentification are described in Units. This column may appear anywhere in the file.",
#>     "TermURL": "https://bids-specification.readthedocs.io/en/stable/common-principles.html#units"
#>   }
#> }
#> 
#> $content:
#>                                           filename                  acq_time
#>                                             <char>                <nanotime>
#> 1:       func/sub-control01_task-nback_bold.nii.gz 1877-06-15T13:45:30+00:00
#> 2:       func/sub-control01_task-motor_bold.nii.gz 1877-06-15T13:55:33+00:00
#> 3: meg/sub-control01_task-rest_split-01_meg.nii.gz 1877-06-15T12:15:27+00:00


# convert existing tabular
tabular <- BIDSTabular(
  data.frame(
    filename = "func/sub-control01_task-nback_bold.nii.gz",
    acq_time = "1877-06-15T13:45:30"
  )
)
tabular <- as_bids_tabular(tabular, cls = BIDSTabularScans)
tabular
#> <BIDS Tabular>[BIDSTabular]
#> $meta:
#> {}
#> 
#> $content:
#>                                     filename            acq_time
#>                                       <char>              <char>
#> 1: func/sub-control01_task-nback_bold.nii.gz 1877-06-15T13:45:30

# save to tsv
tsv <- file.path(tempdir(), "scans.tsv")
paths <- save_bids_tabular(tabular, tsv)

print(paths)
#> $table_path
#> /tmp/RtmpgMwo8s/scans.tsv
#> 
#> $sidecar_path
#> /tmp/RtmpgMwo8s/scans.json
#> 


# use base R to read
read.table(tsv, header = TRUE, na.strings = "n/a")
#>                                    filename            acq_time
#> 1 func/sub-control01_task-nback_bold.nii.gz 1877-06-15T13:45:30

# get sidecar
cat(readLines(paths$sidecar_path), sep = "\n")
#> {}


# clean up
unlink(tsv)
unlink(paths$sidecar_path)


```
