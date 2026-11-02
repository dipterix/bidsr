# Get 'BIDS' entity values from file

Get 'BIDS' entity values from file

## Usage

``` r
get_bids_entity(x, key, value_only = TRUE, ifnotfound = NULL)

get_bids_entity_rules(x)
```

## Arguments

- x:

  'BIDS' file path or parsed object; see 'Examples'

- key:

  entity key

- value_only:

  whether to return the value only; default is true; set to `FALSE` to
  return the entity object

- ifnotfound:

  default value to return is the entity is missing

## Value

'BIDS' entity value or object, depending on `value_only`

## Author

Zhengjia Wang

## Examples

``` r

# Quick usage
get_bids_entity("ieeg/sub-YAB_ses-01_task-AV_ieeg.mat", "sub")
#> [1] "YAB"

get_bids_entity_rules("ieeg/sub-YAB_ses-01_task-AV_channels.tsv")
#> $sub
#> [1] "required" "label"   
#> 
#> $ses
#> [1] "optional" "label"   
#> 
#> $task
#> [1] "required" "label"   
#> 
#> $acq
#> [1] "optional" "label"   
#> 
#> $run
#> [1] "optional" "index"   
#> 
#> $desc
#> [1] "optional" "label"   
#> 

# Full usage
parsed <- parse_path_bids_entity(
    path = "ieeg/sub-YAB_ses-01_task-AV_channels.tsv")

parsed$get_bids_entity("sub")
#> [1] "YAB"
parsed$get_bids_entity_rules()
#> $sub
#> [1] "required" "label"   
#> 
#> $ses
#> [1] "optional" "label"   
#> 
#> $task
#> [1] "required" "label"   
#> 
#> $acq
#> [1] "optional" "label"   
#> 
#> $run
#> [1] "optional" "index"   
#> 
#> $desc
#> [1] "optional" "label"   
#> 

parsed$description
#> [1] "Defined via datatype+suffix: `ieeg/channels` (BIDS version: 1.10.1)"
parsed$entities
#> $sub
#> sub-YAB
#> 
#> $ses
#> ses-01
#> 
#> $task
#> task-AV
#> 
```
