# 'BIDS' uniform resource indicator ('URI') class definition

'BIDS' uniform resource indicator ('URI') class definition

## Usage

``` r
BIDSURI(uri)
```

## Arguments

- uri:

  'URI' string or another 'BIDS-URI' object

## Value

A `BIDSURI` instance.

## Author

Zhengjia Wang

## Examples

``` r

# basic properties
uri <- BIDSURI("bids::sub-01/fmap/sub-01_dir-AP_epi.nii.gz")
uri
#> bids::sub-01/fmap/sub-01_dir-AP_epi.nii.gz
uri$relative_path
#> [1] "sub-01/fmap/sub-01_dir-AP_epi.nii.gz"
uri$dataset_name
#> [1] ""

# set the entire uri
uri$format <- "bids:deriv1:sub-02/anat/sub-02_T1w.nii.gz"
uri
#> bids:deriv1:sub-02/anat/sub-02_T1w.nii.gz

# resolve BIDS URI (partial support)


# resolving a BIDS URI requires dataset_description.json
dataset_description <- get_bids_dataset_description(
  parent_directory = "/path/to/BIDS/folder",
  Name = "A dummy experiments",
  BIDSVersion = "1.6.0",

  DatasetLinks = list(
    "deriv1" = "derivatives/derivative1",
    "phantoms" = "file:///data/phantoms"
  )
)

uri <- BIDSURI("bids::sub-01/fmap/sub-01_dir-AP_epi.nii.gz")
resolved <- resolve_bids_path(uri, dataset_description)

# resolved absolute path
print(resolved)
#> /path/to/BIDS/folder/sub-01/fmap/sub-01_dir-AP_epi.nii.gz

# `raw_resolution` is relative to the parent directory where
# `dataset_description.json` is stored
attr(resolved, "raw_resolution")
#> [1] "sub-01/fmap/sub-01_dir-AP_epi.nii.gz"

uri <- BIDSURI("bids:deriv1:sub-02/anat/sub-02_T1w.nii.gz")
resolved <- resolve_bids_path(uri, dataset_description)

print(resolved)
#> /path/to/BIDS/folder/derivatives/derivative1/sub-02/anat/sub-02_T1w.nii.gz

attr(resolved, "raw_resolution")
#> derivatives/derivative1/sub-02/anat/sub-02_T1w.nii.gz

```
