# 'BIDS' sessions table class

A tabular containing a list of sessions and their metadata. The class is
a child class of
[`BIDSTabular`](https://dipterix.org/bidsr/reference/BIDSTabular.md),
hence see the methods there. The original specification is at
<https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#sessions-file>.

## Usage

``` r
BIDSTabularSessions(content, meta = NULL)
```

## Arguments

- content, meta:

  see
  [`BIDSTabular`](https://dipterix.org/bidsr/reference/BIDSTabular.md)

## Value

A `BIDSTabularSessions` instance inheriting
[`BIDSTabular`](https://dipterix.org/bidsr/reference/BIDSTabular.md).

## Author

Zhengjia Wang

## Examples

``` r



# basic
tabular <- BIDSTabularSessions(data.frame(
  session_id = c("ses-predrug", "ses-postdrug", "ses-followup"),
  acq_time = c(
    "2009-06-15T13:45:30",
    "2009-06-16T13:45:30",
    "2009-06-17T13:45:30"
  ),
  systolic_blood_pressure = c(120, 100, 110)
))
tabular
#> <BIDS Tabular>[BIDSTabular_sessions]
#> $meta:
#> {
#>   "session_id": {
#>     "LongName": "Session ID",
#>     "Description": "[Required, string] A session identifier of the form ses-<label>, matching a session found in the dataset. There MUST be exactly one row for each session. Values in session_id MUST be unique. This column must appear first in the file."
#>   },
#>   "acq_time": {
#>     "LongName": "Acquisition Time",
#>     "Description": "[Optional, string] Acquisition time refers to when the first data point of the first run was acquired. Datetime format and their deidentification are described in Units. This column may appear anywhere in the file.",
#>     "TermURL": "https://bids-specification.readthedocs.io/en/stable/common-principles.html#units"
#>   },
#>   "pathology": {
#>     "LongName": "Pathology",
#>     "Description": "[Recommended, string or number] String value describing the pathology of the sample or type of control. When different from healthy, pathology SHOULD be specified. The pathology may be specified in either samples.tsv or sessions.tsv, depending on whether the pathology changes over time. This column may appear anywhere in the file."
#>   }
#> }
#> 
#> $content:
#>      session_id                  acq_time systolic_blood_pressure
#>          <char>                <nanotime>                   <num>
#> 1:  ses-predrug 2009-06-15T13:45:30+00:00                     120
#> 2: ses-postdrug 2009-06-16T13:45:30+00:00                     100
#> 3: ses-followup 2009-06-17T13:45:30+00:00                     110


# convert existing tabular
tabular <- BIDSTabular(
  data.frame(
    acq_time = "2009-06-15T13:45:30",
    session_id = "ses-predrug",
    systolic_blood_pressure = 120
  )
)
tabular <- as_bids_tabular(tabular, cls = BIDSTabularSessions)
tabular
#> <BIDS Tabular>[BIDSTabular]
#> $meta:
#> {}
#> 
#> $content:
#>               acq_time  session_id systolic_blood_pressure
#>                 <char>      <char>                   <num>
#> 1: 2009-06-15T13:45:30 ses-predrug                     120


# save to tsv
tsv <- file.path(tempdir(), "sessions.tsv")
paths <- save_bids_tabular(tabular, tsv)

print(paths)
#> $table_path
#> /tmp/Rtmp9cc0w1/sessions.tsv
#> 
#> $sidecar_path
#> /tmp/Rtmp9cc0w1/sessions.json
#> 


# use base R to read
read.table(tsv, header = TRUE, na.strings = "n/a")
#>              acq_time  session_id systolic_blood_pressure
#> 1 2009-06-15T13:45:30 ses-predrug                     120

# get sidecar
cat(readLines(paths$sidecar_path), sep = "\n")
#> {}


# clean up
unlink(tsv)
unlink(paths$sidecar_path)


```
