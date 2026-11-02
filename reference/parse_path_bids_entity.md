# Parse 'BIDS' entities from file path

Parse 'BIDS' entities from file path

## Usage

``` r
parse_path_bids_entity(
  path,
  auto_cache = TRUE,
  schema_key = NA,
  bids_version = current_bids_version()
)
```

## Arguments

- path:

  path to the entity file, recommended to input the absolute path or
  relative path from the 'BIDS' root directory

- auto_cache:

  whether to automatically cache the class definition to speed to next
  time; default is true

- schema_key:

  'BIDS' schema key if explicit entity rules is needed

- bids_version:

  'BIDS' version to query the entity rules

## Value

A `'BIDSEntityFile'` instance.

## Author

Zhengjia Wang

## Examples

``` r


path <- "anat/sub-01_chunk-001_t1w.nii.gz"

# --- parse ------------------------------------------------
parsed_filename <- parse_path_bids_entity(path)
parsed_filename
#> anat/sub-01_chunk-001_t1w.nii.gz

parsed_filename$get_bids_entity("sub")
#> [1] "01"

# alternatively
parsed_filename$entities$sub$value
#> [1] "01"

# data type is `anat` imaging
parsed_filename$data_type
#> [1] "anat"

# data is T1-weighted
parsed_filename$suffix
#> [1] "t1w"

# --- usage ------------------------------------------------
# use it as character
file.path("/path/to/bids/dir/sub-01", parsed_filename)
#> [1] "/path/to/bids/dir/sub-01/anat/sub-01_chunk-001_t1w.nii.gz"

# modify
parsed_filename$entities$task <- "special"

# new file path: anat/sub-01_task-special_chunk-001_T1w.nii.gz
parsed_filename
#> anat/sub-01_chunk-001_task-special_t1w.nii.gz

# ---- schema -----------------------------------------------
# get BIDS entity rules
parsed_filename$get_bids_entity("task")
#> [1] "special"

# get entity rules
parsed_filename$get_bids_entity_rules()
#> $sub
#> [1] "required" "label"   
#> 
#> $ses
#> [1] "optional" "label"   
#> 
#> $task
#> [1] "optional" "label"   
#> 
#> $acq
#> [1] "optional" "label"   
#> 
#> $ce
#> [1] "optional" "label"   
#> 
#> $rec
#> [1] "optional" "label"   
#> 
#> $run
#> [1] "optional" "index"   
#> 
#> $echo
#> [1] "optional" "index"   
#> 
#> $part
#> [1] "optional" "label"   
#> 
#> $space
#> [1] "optional" "label"   
#> 
#> $chunk
#> [1] "optional" "index"   
#> 
#> $res
#> [1] "optional" "label"   
#> 
#> $den
#> [1] "optional" "label"   
#> 
#> $desc
#> [1] "optional" "label"   
#> 

```
