
# bidsr

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/dipterix/bidsr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dipterix/bidsr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Brain Imaging Data Structure (`BIDS`) is a standard for organizing neuroimaging and behavioral data (see https://bids.neuroimaging.io/index.html). 
Package `bidsr` aims at providing comprehensive tools to query and manipulate 'BIDS' data files. 
This package is experimental, please see [Road-map](#road-map) below.

## Installation

You can install the development version of `bidsr` like so:

``` r
# install.packages("pak")
pak::pak("dipterix/bidsr")
```

## Road-map

Current milestone:

* Base classes
  - [X] R `S7` base classes and properties
  - [X] tabular data
  - [X] `JSON` side-car
  - [X] `BIDS` entity
* Class definitions for common files
  - Modality agnostic files:
    * [X] `dataset_description.json`
    * [X] `README`
    * [ ] `CITATION.cff`, `CHANGES`, `LICENSE`
    * [X] `participants.tsv`, `participants.json`
    * [X] `samples.tsv`, `samples.json`
    * [X] `phenotype/`
    * [X] Scans file
    * [X] Sessions file
  - Modality specific files:
    * [X] Query files by data type for each subject
    * [X] Use `BIDS` schema to generate file entity rules dynamically
* Query `BIDS` files
  - [X] Get top-level files
  - [X] Analyze and list subject data by types
  - [ ] Find side-car and meta-data information using the `BIDS` inheritance principle

Next milestone:

* Provide read functions to handle common file formats
  - [X] `JSON` format
  - [X] `.tsv` or `.csv` format
  - [ ] `.nii` format
  - [ ] `.mat`, `.edf`, ... (electrophysiology)
* [ ] Provide handler registry to access and process data files

A tentative to-do list:    

* [ ] Modify the path
* [ ] Use `BIDS` schema to validation rules dynamically
* [ ] Validate `BIDS` files

## Example

This example demonstrates high-level tools to query a 'BIDS' project. 
The code requires executing a built-in command to download ['BIDS' example data-sets](https://github.com/bids-standard/bids-examples). 
Please copy-paste-run the following R commands:

``` r
library(bidsr)
example_root <- download_bids_examples()
```

Let's inspect the project `ieeg_motorMiller2007` from the official `BIDS` example repository:

``` r
## Path to `ieeg_motorMiller2007`
project_path <- file.path(example_root, "ieeg_motorMiller2007")


## BIDS project instance
project <- bids_project(path = project_path)


## The top-level `dataset_description.json` and `participants.tsv` 
## can be parsed via
description <- project$get_dataset_description()
participants <- project$get_participants()


## To obtain the values, use `$` operator (same as `.` in `Python`):
description$BIDSVersion


## Given a project path or instance, a `BIDS` subject can be instantiated via 
## the following two ways. 

# Method 1
subject <- bids_subject(project = project, subject_code = "sub-bp")

# Method 2
subject <- bids_subject(project = project_path, subject_code = "bp")

## The official `BIDS` and its extensions support various of data types, 
## such as `anat`, `func`, `meg`, `eeg`, `ieeg`, etc. 
## use `query_modality` method to query the files
ieeg_table <- subject$query_modality("ieeg")
ieeg_table
#>           parsed data_type      suffix extension    sub    ses    acq
#>           <AsIs>    <char>      <char>    <char> <char> <char> <lgcl>
#>  1: sub-bp/s....      ieeg coordsystem      json     bp     01     NA
#>  2: sub-bp/s....      ieeg  electrodes       tsv     bp     01     NA
#>  3: sub-bp/s....      ieeg coordsystem      json     bp     01     NA 
#>  4: sub-bp/s....      ieeg  electrodes       tsv     bp     01     NA 
#>  5: sub-bp/s....      ieeg    channels       tsv     bp     01     NA     
#>  6: sub-bp/s....      ieeg      events       tsv     bp     01     NA    
#>  7: sub-bp/s....      ieeg        ieeg       eeg     bp     01     NA    
#>  8: sub-bp/s....      ieeg        ieeg      json     bp     01     NA    
#>  9: sub-bp/s....      ieeg        ieeg      vhdr     bp     01     NA    
#> 10: sub-bp/s....      ieeg        ieeg      vmrk     bp     01     NA    
#>         space   task    run
#>        <char> <char> <char>
#>  1:      ACPC   <NA>   <NA>
#>  2:      ACPC   <NA>   <NA>
#>  3: Talairach   <NA>   <NA>
#>  4: Talairach   <NA>   <NA>
#>  5:      <NA>  motor      1
#>  6:      <NA>  motor      1
#>  7:      <NA>  motor      1
#>  8:      <NA>  motor      1
#>  9:      <NA>  motor      1
#> 10:      <NA>  motor      1


## The first column contains a list of file instances. Here's an 
## example to read the `ACPC` electrode coordinates:

subset_result <- subset(ieeg_table, space == "ACPC" & suffix == "electrodes")
path_parsed <- subset_result$parsed[[1]]
print(path_parsed)

electrode_path <- file.path(project, path_parsed)
as_bids_tabular(electrode_path)
#> <BIDS Tabular>[bids_tabular]
#> $meta:
#> {}
#> 
#> $content:
#>      name         x           y          z  size    type manufacturer
#>     <int>     <num>       <num>      <num> <int>  <char>       <char>
#>  1:     1 -38.23672  42.9909111  32.116130     4 surface       AdTech
#>  2:     2 -44.21946  32.4059267  34.909389     4 surface       AdTech
#>  3:     3 -45.31461  22.1149697  37.702649     4 surface       AdTech
#>  4:     4 -48.16445  11.2359580  40.201882     4 surface       AdTech
#>  5:     5 -54.12109   0.9450009  42.995141     4 surface       AdTech
#> ---                                                                  
#> 43:    43 -58.09218   5.5024247 -12.134986     4 surface       AdTech
#> 44:    44 -62.06327  -5.0825597  -9.341726     4 surface       AdTech
#> 45:    45 -65.52670 -15.6675440  -7.577562     4 surface       AdTech
#> 46:    46 -65.38842 -27.2816241  -5.372357     4 surface       AdTech
#> 47:    47 -63.00000 -38.6016769  -4.343261     4 surface       AdTech


## `BIDS` entity

## Object `path_parsed` is a file entity class, with the entities parsed

class(path_parsed)
#> [1] "bidsr::bids_entity_file_ieeg_electrodes"
#> [2] "bidsr::bids_entity_file"                
#> [3] "bidsr::bids_class_base"                 
#> [4] "S7_object"

path_parsed$get_entity("space")
#> [1] "ACPC"

## List all available entities

path_parsed$entities
#> $sub
#> sub-bp
#> 
#> $ses
#> ses-01
#> 
#> $acq
#> 
#> 
#> $space
#> space-ACPC


## If supported by schema, the `BIDS` entity rules for the 
## file can be queried via

path_parsed$get_entity_rules()
#> $sub
#> [1] "required" "label"   
#> 
#> $ses
#> [1] "optional" "label"   
#> 
#> $task
#> [1] "prohibited" "label"     
#> 
#> $acq
#> [1] "optional" "label"   
#> 
#> $run
#> [1] "prohibited" "index"     
#> 
#> $space
#> [1] "optional" "label"   
#> 
#> $recording
#> [1] "prohibited" "label"
```




