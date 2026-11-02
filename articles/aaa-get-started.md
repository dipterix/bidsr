# Get Started

Brain Imaging Data Structure (`BIDS`) is a standard for organizing
neuroimaging and behavioral data (see
<https://bids.neuroimaging.io/index.html>).

The goal of package `bidsr` is to provide comprehensive tools to query
and manipulate ‘BIDS’ data files.

This vignette aims to demonstrate high-level tools to query a ‘BIDS’
project. The code requires executing a built-in command to download
[‘BIDS’ example
data-sets](https://github.com/bids-standard/bids-examples). Please
copy-paste-run the following R commands:

``` r

library(bidsr)
example_root <- download_bids_examples()
```

## Overview of a `BIDS` project

Let’s inspect the project `ds000117` from the official `BIDS` example
repository:

``` r

project_path <- file.path(example_root, "ds000117")
project <- bids_project(path = project_path)

print(project)
#> <ds000117>[BIDSProject] at:
#>   /home/runner/.cache/R/bidsr/bids-examples/bids-examples-master/ds000117
#>   - raw-data path: .
#>   - source-data path: sourcedata
#>   - derivative path: derivatives
#> +- Descriptions: ---------------------------------------------------------------
#> | This dataset was obtained from the OpenNeuro project
#> | (https://www.openneuro.org). Accession #: ds000117
#> | 
#> | The same dataset is also available here:
#> | ftp://ftp.mrc-cbu.cam.ac.uk/personal/rik.henson/wakemandg_hensonrn/, but in
#> | a non-BIDS format (which may be easier to download by subject rather than
#> | by modality)
#> | 
#> | Note that it is a subset of the data available on OpenfMRI
#> | (http://www.openfmri.org; Accession #: ds000117).
#> | 
#> | Description: Multi-subject, multi-modal (sMRI+fMRI+MEG+EEG) neuroimaging
#> | dataset on face processing
#> | 
#> | Please cite the following reference if you use these data:
#> | 
#> | Wakeman, D.G. & Henson, R.N. (2015). A multi-subject, multi-modal human
#> | neuroimaging dataset. Sci. Data 2:150001 doi: 10.1038/sdata.2015.1
#> | 
#> | The data have been used in several publications including, for example:
#> | 
#> | Henson, R.N., Abdulrahman, H., Flandin, G. & Litvak, V. (2019). Multimodal
#> | integration of M/EEG and f/MRI data in SPM12. Frontiers in Neuroscience,
#> | Methods, 13, 300.
#> | 
#> | Henson, R.N., Wakeman, D.G., Litvak, V. & Friston, K.J. (2011). A
#> | Parametric Empirical Bayesian framework for the EEG/MEG inverse problem:
#> | generative models for multisubject and multimodal integration. Frontiers in
#> | Human Neuroscience, 5, 76, 1-16.
#> | 
#> | Chapter 42 of the SPM12 manual
#> | (http://www.fil.ion.ucl.ac.uk/spm/doc/manual.pdf)
#> | 
#> | (see
#> | ftp://ftp.mrc-cbu.cam.ac.uk/personal/rik.henson/wakemandg_hensonrn/Publications
#> | for full list), as well as the BioMag2010 data competition and the Kaggle
#> | competition: https://www.kaggle.com/c/decoding-the-human-brain)
#> | 
#> | ==================================================================================
#> | 
#> | func/ ----- Unlike in v1-v3 of this dataset, the first two (dummy) volumes
#> | have now been removed (as stated in *.json), so event onset times correctly
#> | refer to t=0 at start of third volume
#> | 
#> | Note that, owing to scanner error, Subject 10 only has 170 volumes in last
#> | run (Run 9) (hence the BIDS warning of some onsets in events.tsv file being
#> | later than the data)
#> | 
#> | meg/ ---- Three anatomical fiducials were digitized for aligning the MEG
#> | with the MRI: the nasion (lowest depression between the eyes) and the left
#> | and right ears (lowest depression between the tragus and the helix, above
#> | the tragus). This procedure is illustrated here:
#> | http://neuroimage.usc.edu/brainstorm/CoordinateSystems#Subject_Coordinate_System_.28SCS_.2F_CTF.29
#> | and in task-facerecognition_fidinfo.pdf
#> | 
#> | The following triggers are included in the .fif files and are also used in
#> | the “trigger” column of the meg and bold events files:
#> | 
#> | Trigger Label Simplified Label
#> | 
#> | 5 Initial Famous Face FAMOUS 6 Immediate Repeat Famous Face FAMOUS 7
#> | Delayed Repeat Famous Face FAMOUS 13 Initial Unfamiliar Face UNFAMILIAR 14
#> | Immediate Repeat Unfamiliar Face UNFAMILIAR 15 Delayed Repeat Unfamiliar
#> | Face UNFAMILIAR 17 Initial Scrambled Face SCRAMBLED 18 Immediate Repeat
#> | Scrambled Face SCRAMBLED 19 Delayed Repeat Scrambled Face SCRAMBLED
#> | 
#> | stimuli/meg/ ------------ The .bmp files correspond to those described in
#> | the text. There are 6 additional images in this directory, which were used
#> | in the practice experiment to familiarize participants with the task (hence
#> | some more BIDS validator warnings)
#> | 
#> | stimuli/mri/ ------------ The .bmp files correspond to those described in
#> | the text.
#> | 
#> | Defacing -------- Defacing of MPRAGE T1 images was performed by the
#> | submitter. A subset of subjects have given consent for non-defaced versions
#> | to be shared - in which case, please contact rik.henson@mrc-cbu.cam.ac.uk.
#> | 
#> | Quality Control --------------- Mriqc was run on the dataset. Results are
#> | located in derivatives/mriqc. Learn more about it here:
#> | https://mriqc.readthedocs.io/en/latest/
#> | 
#> | Known Issues ------------ N/A
#> | 
#> | Relationship of Subject Numbering relative to other versions of Dataset
#> | ------------
#> | 
#> | There are multiple versions of the dataset available on the web (see notes
#> | above), and these entailed a renumbering of the subjects for various
#> | reasons. Here are all the versions and how to match subjects between them
#> | (plus some rationale and history for different versions):
#> | 
#> | 1. Original Paper (N=19): Wakeman & Henson (2015): doi:10.1038/sdata.2015.1
#> | Number refers to order that tested (and some, eg 4, 7, 13 etc were excluded
#> | for not completing both MRI and MEG sessions)
#> | 
#> | 2. openfMRI, renumbered from paper:
#> | http://openfmri.org/s3-browser/?prefix=ds000117/ds000117_R0.1.1/uncompressed/
#> | Numbers 1-19 just made contiguous
#> | 
#> | 3. FTP subset of N=16: ftp:
#> | ftp://ftp.mrc-cbu.cam.ac.uk/personal/rik.henson/wakemandg_hensonrn/ This
#> | set was used for SPM Courses Designed to illustrate multimodal integration,
#> | so wanted good MRI+MEG+EEG data for all subjects Removed original
#> | subject_01 and subject_06 because bad EEG data; subject_19 because poor EEG
#> | and fMRI data (And renumbered subject_14 for some reason).
#> | 
#> | 4. Current OpenNeuro subset N=16 used for (BIDS):
#> | https://openneuro.org/datasets/ds000117 OpenNeuro was rebranding of
#> | openfMRI, and enforced BIDS format Since this version designed to
#> | illustrate multi-modal BIDS, kept same numbering as FTP
#> | 
#> | W&H2015 openfMRI FTP openNeuro ======== ====== === ======= subject_01
#> | sub001 subject_02 sub002 Sub01 sub-01 subject_03 sub003 Sub02 sub-02
#> | subject_05 sub004 Sub03 sub-03 subject_06 sub005 subject_08 sub006 Sub05
#> | sub-05 subject_09 sub007 Sub06 sub-06 subject_10 sub008 Sub07 sub-07
#> | subject_11 sub009 Sub08 sub-08 subject_12 sub010 Sub09 sub-09 subject_14
#> | sub011 Sub04 sub-04 subject_15 sub012 Sub10 sub-10 subject_16 sub013 Sub11
#> | sub-11 subject_17 sub014 Sub12 sub-12 subject_18 sub015 Sub13 sub-13
#> | subject_19 sub016 subject_23 sub017 Sub14 sub-14 subject_24 sub018 Sub15
#> | sub-15 subject_25 sub019 Sub16 sub-16
```

### File `dataset_description.json`

The top-level data description can be parsed via

``` r

description <- project$get_bids_dataset_description()
```

To obtain the values, use `$` or `@` operator (same as `.` in `Python`):

``` r

description$BIDSVersion
#> [1] "1.8.0"

# or 
description@BIDSVersion
#> [1] "1.8.0"
```

### File `participants.tsv`

File `participants.tsv` is a top-level tabular that lists all the
subjects. This file can be queried via method `get_participants`:

``` r

participants <- project$get_bids_participants()
```

Alternatively, we can read any `tsv` file with `as_bids_tabular`

``` r

participant_path <- file.path(project, "participants.tsv")
as_bids_tabular(participant_path, cls = BIDSTabularParticipants)
#> <BIDS Tabular>[BIDSTabular]
#> $meta:
#> {
#>   "age": {
#>     "LongName": "age",
#>     "Units": "year",
#>     "TermURL": "http://purl.obolibrary.org/obo/PATO_0000011"
#>   },
#>   "sex": {
#>     "LongName": "sex",
#>     "Description": "String value indicating phenotypical sex.",
#>     "Levels": {
#>       "F": {
#>         "Description": "Female",
#>         "TermURL": "http://purl.obolibrary.org/obo/PATO_0000383"
#>       },
#>       "M": {
#>         "Description": "Male",
#>         "TermURL": "http://purl.obolibrary.org/obo/PATO_0000384"
#>       }
#>     },
#>     "TermURL": "http://purl.obolibrary.org/obo/PATO_0001894"
#>   },
#>   "first_ses": {
#>     "LongName": "first session",
#>     "Description": "Indicates which session was the first session for this subject.",
#>     "Levels": {
#>       "meg": {
#>         "Description": "meg was the first session"
#>       },
#>       "mri": {
#>         "Description": "mri was the first session"
#>       }
#>     }
#>   }
#> }
#> 
#> $content:
#>     participant_id   age    sex first_ses
#>             <char> <int> <char>    <char>
#>  1:         sub-01    31      M       meg
#>  2:         sub-02    25      M       meg
#>  3:         sub-03    30      M       meg
#>  4:         sub-04    26      F       meg
#>  5:         sub-05    23      F       meg
#> ---                                      
#> 13:         sub-13    25      F       meg
#> 14:         sub-14    24      F       mri
#> 15:         sub-15    30      M       mri
#> 16:         sub-16    25      M       mri
#> 17:  sub-emptyroom    NA   <NA>      <NA>
```

## Instantiate a `BIDS` subject

Given a project path or instance, a `BIDS` subject can be instantiated
via

``` r

subject <- bids_subject(
  project = project, 
  subject_code = "sub-06"
)

print(subject)
#> <BIDSSubject> `sub-06` (project `ds000117`)
```

All `BIDS` subjects have entity key `sub-` that marks the subject code,
for example, `sub-06` means subject `06`, hence the entity key `sub-`
can be omitted, and `bidsr` accepts input such as `subject_code = "06"`.

If `project` is not instantiated, its path is also acceptable, check
this alternative:

``` r

subject <- bids_subject(
  project = project_path, 
  subject_code = "06"
)
```

### Raw, source, or derivative data

`BIDS` mainly focuses on regulating the raw data, however, it also
supports formatting source data and derivatives. The corresponding paths
can be queried via `resolve_bids_path` method. To resolve the root path
for raw/source data,

``` r

# resolve subject path (raw data by default)
resolve_bids_path(subject)
#> /home/runner/.cache/R/bidsr/bids-examples/bids-examples-master/ds000117/./sub-06

resolve_bids_path(subject, storage = "source")
#> /home/runner/.cache/R/bidsr/bids-examples/bids-examples-master/ds000117/sourcedata/sub-06
```

For derivatives, please specify the derivative names, for example:

``` r

resolve_bids_path(subject, storage = "derivative", prefix = "freesurfer")
#> /home/runner/.cache/R/bidsr/bids-examples/bids-examples-master/ds000117/derivatives/freesurfer/sub-06
```

### Query subject files by types

The official `BIDS` and its extensions support various of data types,
such as `anat`, `func`, `meg`, `eeg`, `ieeg`, etc.

To query the data files by type, use `query_bids` method. The following
example shows all the `anat` data path, and its potential meta files

``` r

query_bids(subject, "anat")
#>                                                           parsed data_type
#>                                                           <AsIs>    <char>
#>  1:     sub-06/ses-mri/anat/sub-06_ses-mri_acq-mprage_T1w.nii.gz      anat
#>  2: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-1_FLASH.nii.gz      anat
#>  3: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-2_FLASH.nii.gz      anat
#>  4: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-3_FLASH.nii.gz      anat
#>  5: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-4_FLASH.nii.gz      anat
#>  6: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-5_FLASH.nii.gz      anat
#>  7: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-6_FLASH.nii.gz      anat
#>  8: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-7_FLASH.nii.gz      anat
#>  9: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-1_FLASH.nii.gz      anat
#> 10: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-2_FLASH.nii.gz      anat
#> 11: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-3_FLASH.nii.gz      anat
#> 12: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-4_FLASH.nii.gz      anat
#> 13: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-5_FLASH.nii.gz      anat
#> 14: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-6_FLASH.nii.gz      anat
#> 15: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-7_FLASH.nii.gz      anat
#>     suffix extension    sub    ses    acq   run  echo
#>     <char>    <char> <char> <char> <char> <int> <int>
#>  1:    T1w    nii.gz     06    mri mprage    NA    NA
#>  2:  FLASH    nii.gz     06    mri   <NA>     1     1
#>  3:  FLASH    nii.gz     06    mri   <NA>     1     2
#>  4:  FLASH    nii.gz     06    mri   <NA>     1     3
#>  5:  FLASH    nii.gz     06    mri   <NA>     1     4
#>  6:  FLASH    nii.gz     06    mri   <NA>     1     5
#>  7:  FLASH    nii.gz     06    mri   <NA>     1     6
#>  8:  FLASH    nii.gz     06    mri   <NA>     1     7
#>  9:  FLASH    nii.gz     06    mri   <NA>     2     1
#> 10:  FLASH    nii.gz     06    mri   <NA>     2     2
#> 11:  FLASH    nii.gz     06    mri   <NA>     2     3
#> 12:  FLASH    nii.gz     06    mri   <NA>     2     4
#> 13:  FLASH    nii.gz     06    mri   <NA>     2     5
#> 14:  FLASH    nii.gz     06    mri   <NA>     2     6
#> 15:  FLASH    nii.gz     06    mri   <NA>     2     7
```

To use fine-grained search parameters, replace `'anat'` with a list of
search parameters.

``` r

query_bids(subject, list(
  # dataset to filter, choices are raw, source, or derivative
  storage = "raw",
  
  # include JSON sidecars; default is `FALSE`  
  sidecars = FALSE,
  
  # set to `NULL` to include all data types
  data_types = "anat",
  
  # filter all suffixes
  suffixes = NULL
))
#>                                                           parsed data_type
#>                                                           <AsIs>    <char>
#>  1:     sub-06/ses-mri/anat/sub-06_ses-mri_acq-mprage_T1w.nii.gz      anat
#>  2: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-1_FLASH.nii.gz      anat
#>  3: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-2_FLASH.nii.gz      anat
#>  4: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-3_FLASH.nii.gz      anat
#>  5: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-4_FLASH.nii.gz      anat
#>  6: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-5_FLASH.nii.gz      anat
#>  7: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-6_FLASH.nii.gz      anat
#>  8: sub-06/ses-mri/anat/sub-06_ses-mri_run-1_echo-7_FLASH.nii.gz      anat
#>  9: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-1_FLASH.nii.gz      anat
#> 10: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-2_FLASH.nii.gz      anat
#> 11: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-3_FLASH.nii.gz      anat
#> 12: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-4_FLASH.nii.gz      anat
#> 13: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-5_FLASH.nii.gz      anat
#> 14: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-6_FLASH.nii.gz      anat
#> 15: sub-06/ses-mri/anat/sub-06_ses-mri_run-2_echo-7_FLASH.nii.gz      anat
#>     suffix extension    sub    ses    acq   run  echo
#>     <char>    <char> <char> <char> <char> <int> <int>
#>  1:    T1w    nii.gz     06    mri mprage    NA    NA
#>  2:  FLASH    nii.gz     06    mri   <NA>     1     1
#>  3:  FLASH    nii.gz     06    mri   <NA>     1     2
#>  4:  FLASH    nii.gz     06    mri   <NA>     1     3
#>  5:  FLASH    nii.gz     06    mri   <NA>     1     4
#>  6:  FLASH    nii.gz     06    mri   <NA>     1     5
#>  7:  FLASH    nii.gz     06    mri   <NA>     1     6
#>  8:  FLASH    nii.gz     06    mri   <NA>     1     7
#>  9:  FLASH    nii.gz     06    mri   <NA>     2     1
#> 10:  FLASH    nii.gz     06    mri   <NA>     2     2
#> 11:  FLASH    nii.gz     06    mri   <NA>     2     3
#> 12:  FLASH    nii.gz     06    mri   <NA>     2     4
#> 13:  FLASH    nii.gz     06    mri   <NA>     2     5
#> 14:  FLASH    nii.gz     06    mri   <NA>     2     6
#> 15:  FLASH    nii.gz     06    mri   <NA>     2     7
```

Here is another example that filters derivative files that are `BIDS`
compliant.

``` r

query_bids(subject, list(
  # filter derivatives
  storage = "derivative",
  
  # filter `derivatives/meg_derivatives` folder
  prefix = "meg_derivatives",

  # include JSON sidecars
  sidecars = TRUE,
  
  # set to `NULL` to include all data types
  data_types = NULL,
  
  # only keep files with *_meg/log.* suffixes
  suffixes = c("meg", "log")
))
#>                                                                                                         parsed
#>                                                                                                         <AsIs>
#>  1:           derivatives/meg_derivatives/sub-06/ses-meg/sub-06_ses-meg_task-facerecognition_proc-sss_meg.json
#>  2:       derivatives/meg_derivatives/sub-06/ses-meg/meg/sub-06_ses-meg_task-facerecognition_proc-sss_meg.json
#>  3: derivatives/meg_derivatives/sub-06/ses-meg/meg/sub-06_ses-meg_task-facerecognition_run-01_proc-sss_log.txt
#>  4: derivatives/meg_derivatives/sub-06/ses-meg/meg/sub-06_ses-meg_task-facerecognition_run-01_proc-sss_meg.fif
#>  5: derivatives/meg_derivatives/sub-06/ses-meg/meg/sub-06_ses-meg_task-facerecognition_run-02_proc-sss_log.txt
#>  6: derivatives/meg_derivatives/sub-06/ses-meg/meg/sub-06_ses-meg_task-facerecognition_run-02_proc-sss_meg.fif
#>  7: derivatives/meg_derivatives/sub-06/ses-meg/meg/sub-06_ses-meg_task-facerecognition_run-03_proc-sss_log.txt
#>  8: derivatives/meg_derivatives/sub-06/ses-meg/meg/sub-06_ses-meg_task-facerecognition_run-03_proc-sss_meg.fif
#>  9: derivatives/meg_derivatives/sub-06/ses-meg/meg/sub-06_ses-meg_task-facerecognition_run-04_proc-sss_log.txt
#> 10: derivatives/meg_derivatives/sub-06/ses-meg/meg/sub-06_ses-meg_task-facerecognition_run-04_proc-sss_meg.fif
#> 11: derivatives/meg_derivatives/sub-06/ses-meg/meg/sub-06_ses-meg_task-facerecognition_run-05_proc-sss_log.txt
#> 12: derivatives/meg_derivatives/sub-06/ses-meg/meg/sub-06_ses-meg_task-facerecognition_run-05_proc-sss_meg.fif
#> 13: derivatives/meg_derivatives/sub-06/ses-meg/meg/sub-06_ses-meg_task-facerecognition_run-06_proc-sss_log.txt
#> 14: derivatives/meg_derivatives/sub-06/ses-meg/meg/sub-06_ses-meg_task-facerecognition_run-06_proc-sss_meg.fif
#>     data_type suffix extension    sub    ses            task   proc   run
#>        <char> <char>    <char> <char> <char>          <char> <char> <int>
#>  1:     _root    meg      json     06    meg facerecognition    sss    NA
#>  2:       meg    meg      json     06    meg facerecognition    sss    NA
#>  3:       meg    log       txt     06    meg facerecognition    sss     1
#>  4:       meg    meg       fif     06    meg facerecognition    sss     1
#>  5:       meg    log       txt     06    meg facerecognition    sss     2
#>  6:       meg    meg       fif     06    meg facerecognition    sss     2
#>  7:       meg    log       txt     06    meg facerecognition    sss     3
#>  8:       meg    meg       fif     06    meg facerecognition    sss     3
#>  9:       meg    log       txt     06    meg facerecognition    sss     4
#> 10:       meg    meg       fif     06    meg facerecognition    sss     4
#> 11:       meg    log       txt     06    meg facerecognition    sss     5
#> 12:       meg    meg       fif     06    meg facerecognition    sss     5
#> 13:       meg    log       txt     06    meg facerecognition    sss     6
#> 14:       meg    meg       fif     06    meg facerecognition    sss     6
```

## Filter & get `BIDS` entity

Here is an example of filtering `func` event files with `BIDS` entity
`run=02` The first column contains a list of file instances. Here’s an
example to read the `ACPC` electrode coordinates:

``` r

filter_result <- query_bids(subject, list(
  storage = "raw",
  sidecars = FALSE,
  
  data_types = "func",
  suffixes = "events",
  
  # use R "formula" to filter entities
  entity_filters = list(

    # entity_key ~ expression returning TRUE/FALSE
    # When filtering the entities, `entity_key` will be
    # replaced with its value
    run ~ as.integer(run) == 2
  )
))
filter_result
#>                                                                       parsed
#>                                                                       <AsIs>
#> 1: sub-06/ses-mri/func/sub-06_ses-mri_task-facerecognition_run-02_events.tsv
#>    data_type suffix extension    sub    ses            task   run
#>       <char> <char>    <char> <char> <char>          <char> <int>
#> 1:      func events       tsv     06    mri facerecognition     2
```

The first column `"filter_result"` is the parsed file object with entity
information:

``` r

event_file <- filter_result$parsed[[1]]
event_file
#> sub-06/ses-mri/func/sub-06_ses-mri_task-facerecognition_run-02_events.tsv
```

To get entity values, use `get_bids_entity`:

``` r

get_bids_entity(event_file, "task")
#> [1] "facerecognition"
```

If supported by schema, the `BIDS` entity rules for the file can be
queried via `get_bids_entity_rules(event_file)` method

Object `event_file` is relative to the project root, hence can be read
by joining the project root path

``` r

event_path <- file.path(project, event_file)

# or 
event_path <- resolve_bids_path(project, format(event_file))

as_bids_tabular(event_path)
#> <BIDS Tabular>[BIDSTabular]
#> $meta:
#> {}
#> 
#> $content:
#>        onset duration circle_duration  stim_type trigger button_pushed
#>        <num>    <num>           <num>     <char>   <int>         <int>
#>   1:   0.000    0.897           0.596 UNFAMILIAR      13             4
#>   2:   3.207    0.895           0.549 UNFAMILIAR      14             4
#>   3:   6.514    0.959           0.547  SCRAMBLED      17             7
#>   4:   9.738    0.850           0.495  SCRAMBLED      18             7
#>   5:  12.895    0.988           0.548     FAMOUS       5             4
#>  ---                                                                  
#>  96: 385.073    0.935           0.439 UNFAMILIAR      13             4
#>  97: 388.331    0.944           0.551 UNFAMILIAR      13             4
#>  98: 391.455    0.988           0.417     FAMOUS       5             7
#>  99: 394.711    0.912           0.498     FAMOUS       6             7
#> 100: 397.401    0.012           0.000       <NA>     999             0
#>      response_time       stim_file
#>              <num>          <char>
#>   1:         0.849   func/u020.bmp
#>   2:         0.729   func/u020.bmp
#>   3:         1.197   func/s030.bmp
#>   4:         0.695   func/s030.bmp
#>   5:         1.075   func/f027.bmp
#>  ---                              
#>  96:         0.884   func/u031.bmp
#>  97:         1.183   func/u029.bmp
#>  98:         0.869   func/f021.bmp
#>  99:         1.200   func/f021.bmp
#> 100:         0.000 func/Circle.bmp
```
