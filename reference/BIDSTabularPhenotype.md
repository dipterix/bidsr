# 'BIDS' phenotype and assessment table class

A tabular containing a list of phenotype & assessment, with their
metadata. The class is a child class of
[`BIDSTabular`](https://dipterix.org/bidsr/reference/BIDSTabular.md),
hence see the methods there. The original specification is at
<https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#phenotypic-and-assessment-data>.

## Usage

``` r
get_bids_phenotype_data(x, ...)

BIDSTabularPhenotype(content, meta = NULL)
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

A `BIDSTabularPhenotype` instance inheriting
[`BIDSTabular`](https://dipterix.org/bidsr/reference/BIDSTabular.md).

## Author

Zhengjia Wang

## Examples

``` r



BIDSTabularPhenotype(
  meta = list(
    MeasurementToolMetadata = list(
      Description = "Adult ADHD Clinical Diagnostic Scale V1.2",
      TermURL = "https://www.cognitiveatlas.org/task/id/trm_5586ff878155d"
    ),
    adhd_b = list(
      Description = "B. CHILDHOOD ONSET OF ADHD (PRIOR TO AGE 7)",
      Levels = list(
        "1" = "YES",
        "2" = "NO"
      )
    ),
    adhd_c_dx = list(
      Description = "As child met A, B, C, D, E and F diagnostic criteria",
      Levels = list(
        "1" = "YES",
        "2" = "NO"
      )
    )
  ),
  content = data.frame(
    MeasurementToolMetadata = c(2, 3, 4),
    adhd_b = c(1, 2, 1),
    adhd_c_dx = c(2, 1, 2)
  )
)
#> <BIDS Tabular>[BIDSTabular_phenotype]
#> $meta:
#> {
#>   "MeasurementToolMetadata": {
#>     "Description": "Adult ADHD Clinical Diagnostic Scale V1.2",
#>     "TermURL": "https://www.cognitiveatlas.org/task/id/trm_5586ff878155d"
#>   },
#>   "adhd_b": {
#>     "Description": "B. CHILDHOOD ONSET OF ADHD (PRIOR TO AGE 7)",
#>     "Levels": {
#>       "1": "YES",
#>       "2": "NO"
#>     }
#>   },
#>   "adhd_c_dx": {
#>     "Description": "As child met A, B, C, D, E and F diagnostic criteria",
#>     "Levels": {
#>       "1": "YES",
#>       "2": "NO"
#>     }
#>   },
#>   "Derivative": {
#>     "LongName": "Derivative",
#>     "Description": "[Optional, boolean] Indicates that values in the corresponding column are transformations of values from other columns (for example a summary score based on a subset of items in a questionnaire).",
#>     "Levels": {
#>       "true": "true",
#>       "false": "false"
#>     }
#>   }
#> }
#> 
#> $content:
#>    MeasurementToolMetadata adhd_b adhd_c_dx
#>                      <num>  <num>     <num>
#> 1:                       2      1         2
#> 2:                       3      2         1
#> 3:                       4      1         2

```
