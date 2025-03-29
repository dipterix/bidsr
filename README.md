
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bidsr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/dipterix/bidsr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dipterix/bidsr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Brain Imaging Data Structure (`BIDS`) is a standard for organizing
neuroimaging and behavioral data (see
<https://bids.neuroimaging.io/index.html>). Package `bidsr` aims at
providing comprehensive tools to query and manipulate ‘BIDS’ data files.
This package is experimental, please see [Road-map](#road-map) below.

## Installation

You can install the development version of `bidsr` like so:

``` r
# install.packages("pak")
pak::pak("dipterix/bidsr")
```

## Example

This example demonstrates high-level tools to query a ‘BIDS’ project.
The code requires executing a built-in command to download [‘BIDS’
example data-sets](https://github.com/bids-standard/bids-examples).
Please copy-paste-run the following R commands:

``` r
library(bidsr)
example_root <- download_bids_examples()
```

Let’s inspect the project `ieeg_motorMiller2007` from the official
`BIDS` example repository:

``` r
## Path to `ieeg_motorMiller2007`
project_path <- file.path(example_root, "ieeg_motorMiller2007")

## BIDS project instance
project <- bids_project(path = project_path)

## The top-level `dataset_description.json` and `participants.tsv` 
## can be parsed via
description <- project$get_bids_dataset_description()
participants <- project$get_bids_participants()


## To obtain the values, use `$` operator (same as `.` in `Python`):
description$BIDSVersion
#> [1] "1.0.2"

## Given a project path or instance, a `BIDS` subject can be instantiated via 
## the following two ways. 

# Method 1
subject <- bids_subject(project = project, subject_code = "sub-bp")

# Method 2
subject <- bids_subject(project = project_path, subject_code = "bp")

## The official `BIDS` and its extensions support various of data types, 
## such as `anat`, `func`, `meg`, `eeg`, `ieeg`, etc. 
## use `query_modality` method to query the files
ieeg_table <- query_bids(subject, "ieeg")
print(ieeg_table)
#>          parsed data_type     suffix extension    sub    ses     space   task
#>          <AsIs>    <char>     <char>    <char> <char> <char>    <char> <char>
#> 1: sub-bp/s....      ieeg electrodes       tsv     bp     01      ACPC   <NA>
#> 2: sub-bp/s....      ieeg electrodes       tsv     bp     01 Talairach   <NA>
#> 3: sub-bp/s....      ieeg   channels       tsv     bp     01      <NA>  motor
#> 4: sub-bp/s....      ieeg     events       tsv     bp     01      <NA>  motor
#> 5: sub-bp/s....      ieeg       ieeg       eeg     bp     01      <NA>  motor
#> 6: sub-bp/s....      ieeg       ieeg      vhdr     bp     01      <NA>  motor
#> 7: sub-bp/s....      ieeg       ieeg      vmrk     bp     01      <NA>  motor
#>      run
#>    <int>
#> 1:    NA
#> 2:    NA
#> 3:     1
#> 4:     1
#> 5:     1
#> 6:     1
#> 7:     1


## The first column contains a list of file instances. Here's an 
## example to read the `ACPC` electrode coordinates:
subset_result <- subset(ieeg_table, space == "ACPC" & suffix == "electrodes")
print(subset_result)
#>          parsed data_type     suffix extension    sub    ses  space   task
#>          <AsIs>    <char>     <char>    <char> <char> <char> <char> <char>
#> 1: sub-bp/s....      ieeg electrodes       tsv     bp     01   ACPC   <NA>
#>      run
#>    <int>
#> 1:    NA

# Get parsed 'BIDS' file object
path_parsed <- subset_result$parsed[[1]]
print(path_parsed)
#> sub-bp/ses-01/ieeg/sub-bp_ses-01_space-ACPC_electrodes.tsv

electrode_path <- file.path(project, path_parsed)
as_bids_tabular(electrode_path)
#> <BIDS Tabular>[BIDSTabular]
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
#> [1] "bidsr::BIDSEntityFile_ieeg_electrodes"
#> [2] "bidsr::BIDSEntityFile"                
#> [3] "bidsr::BIDSClassBase"                 
#> [4] "S7_object"

path_parsed$get_bids_entity("space")
#> [1] "ACPC"

## List all available entities
path_parsed$entities
#> $sub
#> sub-bp
#> 
#> $ses
#> ses-01
#> 
#> $space
#> space-ACPC

## If supported by schema, the `BIDS` entity rules for the 
## file can be queried via
path_parsed$get_bids_entity_rules()
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
#> $run
#> [1] "optional" "index"   
#> 
#> $space
#> [1] "optional" "label"   
#> 
#> $desc
#> [1] "optional" "label"
```
