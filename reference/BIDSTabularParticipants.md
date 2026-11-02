# 'BIDS' participant table class

A tabular containing a list of participants and their demographics. The
class is a child class of
[`BIDSTabular`](https://dipterix.org/bidsr/reference/BIDSTabular.md),
hence see the methods there. The original specification is at
<https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#participants-file>.

## Usage

``` r
get_bids_participants(x, ...)

BIDSTabularParticipants(content, meta = NULL)
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

A `BIDSTabularParticipants` instance inheriting
[`BIDSTabular`](https://dipterix.org/bidsr/reference/BIDSTabular.md).

## Author

Zhengjia Wang

## Examples

``` r



# basic
tabular <- BIDSTabularParticipants(
  data.frame(
    participant_id = "sub-001"
  )
)
tabular
#> <BIDS Tabular>[BIDSTabular_participants]
#> $meta:
#> {
#>   "participant_id": {
#>     "LongName": "Participant ID",
#>     "Description": "A participant identifier of the form sub-<label>, matching a participant entity found in the dataset",
#>     "TermURL": "https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#participants-file"
#>   },
#>   "species": {
#>     "LongName": "Species",
#>     "Description": "The species column SHOULD be a binomial species name from the NCBI Taxonomy (for example, homo sapiens, mus musculus, rattus norvegicus). For backwards compatibility, if species is absent, the participant is assumed to be homo sapiens.",
#>     "TermURL": "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi"
#>   },
#>   "age": {
#>     "LongName": "Subject age",
#>     "Description": "Numeric value in years (float or integer value). It is recommended to tag participant ages that are 89 or higher as 89+, for privacy purposes"
#>   },
#>   "sex": {
#>     "LongName": "Sex",
#>     "Description": "String value indicating phenotypical sex, one of \"male\", \"female\", \"other\"",
#>     "Levels": {
#>       "male": "male",
#>       "female": "female",
#>       "other": "other"
#>     }
#>   },
#>   "handedness": {
#>     "LongName": "Subject handedness",
#>     "Description": "String value indicating one of \"left\", \"right\", \"ambidextrous\"",
#>     "Levels": {
#>       "left": "left",
#>       "right": "right",
#>       "ambidextrous": "ambidextrous"
#>     },
#>     "TermURL": "https://bids-specification.readthedocs.io/en/stable/glossary.html#objects.columns.handedness"
#>   },
#>   "strain": {
#>     "LongName": "Strain",
#>     "Description": "For species different from homo sapiens, string value indicating the strain of the species, for example: C57BL/6J"
#>   },
#>   "strain_rrid": {
#>     "LongName": "Strain RRID",
#>     "Description": "For species different from homo sapiens, research resource identifier (RRID) of the strain of the species",
#>     "TermURL": "https://rrid.site/data/source/nlx_154697-1/search"
#>   }
#> }
#> 
#> $content:
#>    participant_id      species
#>            <char>       <char>
#> 1:        sub-001 homo sapiens


# Run `download_bids_examples()` first
examples <- download_bids_examples(test = TRUE)
if(!isFALSE(examples)) {

  file <- file.path(examples, "ieeg_epilepsy_ecog", "participants.tsv")

  # read tabular as BIDSTabularParticipants
  as_bids_tabular(file, cls = BIDSTabularParticipants)

  # convert existing tabular
  tabular <- BIDSTabular(
    data.frame(
      participant_id = "sub-001"
    )
  )
  tabular <- as_bids_tabular(tabular, cls = BIDSTabularParticipants)

  # save to tsv
  tsv <- file.path(tempdir(), "participants.tsv")
  paths <- save_bids_tabular(tabular, tsv)
  print(paths)

  # use base R to read
  read.table(tsv, header = TRUE, na.strings = "n/a")

  # get sidecar
  cat(readLines(paths$sidecar_path), sep = "\n")

  unlink(tsv)
  unlink(paths$sidecar_path)
}
```
