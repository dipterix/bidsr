# Class definition for 'BIDS' meta-data `'GeneratedBy'`

See definition at
<https://bids-specification.readthedocs.io/en/stable/glossary.html#objects.metadata.GeneratedBy>

## Usage

``` r
BIDSDatasetGeneratedBy(
  Name = character(0),
  Version = character(0),
  Description = character(0),
  CodeURL = character(0),
  Container = list()
)
```

## Arguments

- Name:

  (character, required) name of the pipeline or process that generated
  the outputs.

- Version:

  (character, optional) version of the pipeline

- Description:

  (character, optional) plain-text description of the pipeline or
  process that generated the outputs.

- CodeURL:

  (character, optional) 'URL' where the code used to generate the data
  may be found.

- Container:

  (character, optional) Used to specify the location and relevant
  attributes of software container image used to produce the data. Valid
  keys in this object include type, tag 'URL' with string values.
  Package `'bidsr'` does not check what's inside of this entry.

## Value

Instantiated object of class `BIDSDatasetGeneratedBy`

## Author

Zhengjia Wang

## Examples

``` r

x <- BIDSDatasetGeneratedBy(
  Name = "RAVE Team",
  Version = "0.0.1",
  Container = list(
    Type = "docker",
    Tag = "rave-ieeg/rave-pipelines:0.0.1"
  )
)

x
#> {
#>   "Name": "RAVE Team",
#>   "Version": "0.0.1",
#>   "Container": {
#>     "Type": "docker",
#>     "Tag": "rave-ieeg/rave-pipelines:0.0.1"
#>   }
#> }

x$Version <- "0.0.2"

# convert to basic list
as.list(x)
#> $Name
#> [1] "RAVE Team"
#> 
#> $Version
#> [1] "0.0.2"
#> 
#> $Container
#> $Container$Type
#> [1] "docker"
#> 
#> $Container$Tag
#> [1] "rave-ieeg/rave-pipelines:0.0.1"
#> 
#> 

# get JSON string
format(x)
#> {
#>   "Name": "RAVE Team",
#>   "Version": "0.0.2",
#>   "Container": {
#>     "Type": "docker",
#>     "Tag": "rave-ieeg/rave-pipelines:0.0.1"
#>   }
#> } 
```
