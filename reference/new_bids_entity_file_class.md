# Class generator for 'BIDS' file class with entities

Low-level function to generate file name definitions with entity
constraints; use
[`parse_path_bids_entity`](https://dipterix.org/bidsr/reference/parse_path_bids_entity.md)
instead. The specification is at
<https://bids-specification.readthedocs.io/en/stable/common-principles.html#filenames>.

## Usage

``` r
new_bids_entity_file_class(
  name,
  data_type,
  suffix,
  schema_key = NA,
  bids_version = current_bids_version()
)
```

## Arguments

- name:

  class name

- data_type:

  'BIDS' file data type

- suffix:

  file suffix

- schema_key:

  schema key if explicit entity rules are required

- bids_version:

  'BIDS' version to query the entity rules

## Value

A class definition with proper entity constraints according to
`data_type`-`suffix` combinations, or a specific `schema_key`. The
function rarely needs to be called directly unless the schema key is
missing from the specification.

## Author

Zhengjia Wang

## Examples

``` r


# see full table at BIDS specification
# en/stable/appendices/entity-table.html#behavioral-data
#
# generate class definition for "Behavioral Data"
# Entity: Subject Session Task Acquisition Run Recording
# Format:
#   sub-<label> ses-<label> task-<label>
#   acq-<label> run-<index> recording-<label>
# suffix: events
# requirement: REQUIRED OPTIONAL REQUIRED OPTIONAL OPTIONAL
#


# ---- Basic usage ----------------------------------------
behavior_event_file_def <- new_bids_entity_file_class(
  name = "BIDSEntityFile_beh_events",
  data_type = "beh",
  suffix = "events"
)

file1 <- behavior_event_file_def(
  parent_directory = "sub-001/beh",
  sub = "001", task = "test", .extension = "tsv")

print(file1)
#> sub-001/beh/sub-001_task-test_events.tsv

file.path("root/to/path", file1)
#> [1] "root/to/path/sub-001/beh/sub-001_task-test_events.tsv"

# How the entities are parsed?
file1$description
#> [1] "Defined via datatype+suffix: `beh/events` (BIDS version: 1.10.1)"

# get entity values
file1$get_bids_entity("task")
#> [1] "test"

# parent directory
file1$parent_directory
#> [1] "sub-001/beh"

file1$entities$run$value
#> NULL

# set entity values
file1$entities$run <- 2
file1$entities$run$index_format <- "%03d"

file1$entities$blahblah <- "haha"

file1
#> sub-001/beh/sub-001_task-test_run-002_blahblah-haha_events.tsv

# Relaxed entity rules generated from schema
# `rules.files.raw.task.events` and
# `rules.files.deriv.preprocessed_data.task_events_common`
get_bids_entity_rules(file1)
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

# ---- Using BIDS schema key for specific version ------------------------
bids_version <- "1.10.1"
behavior_event_file_def <- new_bids_entity_file_class(
  name = "BIDSEntityFile_beh_events",
  data_type = "beh",
  suffix = "events",
  schema_key = "rules.files.raw.task.events",
  bids_version = bids_version
)

file2 <- behavior_event_file_def(
  parent_directory = "sub-001/beh",
  sub = "001", task = "test", .extension = "tsv")

file2$description
#> [1] "Defined via schema_key: `rules.files.raw.task.events` (BIDS version: 1.10.1)"

# `desc` is no longer listed in the rules here
get_bids_entity_rules(file2)
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


```
