# Class definition of 'BIDS' data-set description

See
<https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#dataset_descriptionjson>
for specification.

## Usage

``` r
get_bids_dataset_description(x, parent_directory, ...)

BIDSDatasetDescription(
  Name = character(0),
  BIDSVersion = character(0),
  DatasetLinks = list(),
  HEDVersion = character(0),
  DatasetType = character(0),
  License = character(0),
  Authors = character(0),
  GeneratedBy = list(),
  SourceDatasets = list(),
  Acknowledgements = character(0),
  HowToAcknowledge = character(0),
  Funding = character(0),
  EthicsApprovals = character(0),
  ReferencesAndLinks = character(0),
  DatasetDOI = character(0),
  parent_directory = character(0)
)
```

## Arguments

- x:

  R object to be interpreted as 'BIDS' data description; default support
  list, path to the `'json'` file, `'json'` string, etc.

- parent_directory:

  parent directory where the file `'dataset_description.json'` is
  stored. This input is ignored if `x` is the path to
  `'dataset_description.json'`, otherwise is a must.

- ...:

  passed to methods

- Name:

  (required, string) Name of the data-set.

- BIDSVersion:

  (required, string) The version of the BIDS standard that was used.

- DatasetLinks:

  (required if 'BIDS-URI' is used) Used to map a given data-set name
  from a 'BIDS-URI' of the form
  `bids:<dataset-name>:path/within/dataset` to a local or remote
  location.

- HEDVersion:

  (recommended strings) The version of the 'HED' schema used to validate
  'HED' tags for study. May include a single schema or a base schema and
  one or more library schema.

- DatasetType:

  (recommended string) Must be one of `"raw"` or `"derivative"`; package
  `bidsr` automatically assigns `"raw"` is not given.

- License:

  (recommended string) The license for the data-set

- Authors:

  (recommended strings) Vector of individuals who contributed to the
  creation/curation of the data-set

- GeneratedBy:

  (recommended) will be converted to
  [`BIDSDatasetGeneratedBy`](https://dipterix.org/bidsr/reference/BIDSDatasetGeneratedBy.md)

- SourceDatasets:

  Used to specify the locations and relevant attributes of all source
  data-sets. Valid keys in each object include `"URL"`, `"DOI"`, and
  `"Version"` with string values; Package `bidsr` does not check the
  names

- Acknowledgements:

  (optional string) Text acknowledging contributions of individuals or
  institutions beyond those listed in Authors or Funding.

- HowToAcknowledge:

  (optional string) Text containing instructions on how researchers
  using this dataset should acknowledge the original authors. This field
  can also be used to define a publication that should be cited in
  publications that use the dataset.

- Funding:

  (optional strings) List of sources of funding (grant numbers).

- EthicsApprovals:

  (optional strings) List of ethics committee approvals of the research
  protocols and/or protocol identifiers.

- ReferencesAndLinks:

  (optional strings) List of references to publications that contain
  information on the data-set. A reference may be textual or a URI.

- DatasetDOI:

  (optional string) The Digital Object Identifier of the data-set (not
  the corresponding paper). `'DOIs'` should be expressed as a valid
  'URI'

## Value

A `S7` description object that contains all the fields describing the
data set; see 'Examples' for usages.

## Author

Zhengjia Wang

## Examples

``` r

# ---- Manually enter entries ----------------------------------------
dataset_description <- BIDSDatasetDescription(
  # a parent directory is mandatory as it defines what data
  # dataset_description.json applies to

  parent_directory = "/path/to/BIDS/folder",

  Name = "A dummy experiments",
  BIDSVersion = "1.6.0",
  License = "CC0",
  Authors = c("Zhengjia Wang"),
  Acknowledgements = c(
    "Package `bidsr` is a 3rd-party BIDS reader developed by",
    "a RAVE (https://rave.wiki) team member with procrastination."
  ),
  HowToAcknowledge = c(
    "Please cite this paper:",
    "https://doi.org/10.1016/j.neuroimage.2020.117341"
  ),
  Funding = c(
    "NIH R01MH133717",
    "NIH U01NS113339",
    "NIH 1R24MH117529"
  ),
  ReferencesAndLinks = c(
    "https://rave.wiki"
  ),
  DatasetDOI = "https://doi.org/10.1016/j.neuroimage.2020.117341",
  HEDVersion = "8.0.0",
  GeneratedBy = list(
    list(
      Name = "Dipterix",
      Version = "0.0.1",
      Container = list(
        Type = "r-package",
        Tag = "dipterix/bidsr:0.0.1"
      )
    )
  )
)

# access the information
dataset_description$License
#> [1] "CC0"

dataset_description$GeneratedBy[[1]]$Container
#> $Type
#> [1] "r-package"
#> 
#> $Tag
#> [1] "dipterix/bidsr:0.0.1"
#> 

# ---- Read from file ---------------------------------------------

# Run `download_bids_examples()` first
examples <- download_bids_examples(test = TRUE)
if(!isFALSE(examples)) {
  example_descr <- file.path(
    examples, "ieeg_epilepsy_ecog", "dataset_description.json")

  x <- get_bids_dataset_description(example_descr)
  x

  # ---- Formatting --------------------------------------------------
  # convert to R list (use recursive to expand field `GeneratedBy`)
  as.list(x, recursive = TRUE)

  # JSON string
  format(x)
}
```
