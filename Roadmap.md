## Roadmap

### Phase 1: read BIDS data

> Implement basic data structures (classes) to map to BIDS raw & source dataset.

Something like:

```r
# Project
bproj = bidsr::bids_project("/path/to/project/folder")

bproj$description  # dataset_description.json
bproj$participants # participants.json

# Subject
bsubj = bproj$get_subject("subject_label")
bsubj = bidsr::as_bids_subject("/path/to/subject/folder")
bsubj = bidsr::as_bids_subject("subject_label", project = <string/bids_project>)

bsubj$rawdata
bsubj$sourcedata
bsubj$derivatives
bsubj$phenotype
bsubj$stimuli

# BIDS compliant data container
rawdata = bsubj$rawdata
rawdata$bids_compliant  # TRUE
ieegdata = rawdata$ieeg
ieegdata$datatype # 'ieeg'

# Query data
query_result = bids_query(
  bproj, sub = "01", datatype = "ieeg",
  task = ...
)
query_result = bids_query(
  ieegdata,    # implicit `bproj`, `sub-01`, `ieeg`,
  acq = ...
)
query_result = bproj[sub = "01", ...]

# Snapshot query data
as.data.frame(query_result)    # Dataset x entities + suffix
query_result$count("channel")  # How many *_channel results (prefix-only)
query_result$suffixes

ieeg_data <- query_result$ieeg[[1]]  # get the first wrapper containing iEEG signals

# BIDS data file
ieeg_data$metadata # combined tsv + json
ieeg_data$path     # resolved path, BIDS URI, original path
bids_data_content(ieeg_data) # load data into memory
```

### Phase 2: migrate RAVE into BIDS derivative path so RAVE can read form BIDS

When RAVE `raveio::RAVEProject` finds `dataset_description.json` in its folder, then RAVE activates its BIDS mode for this project.

The following interface will be provided

```r
proj = raveio::RAVEProject$new(...)
proj$convert_to_bids()    # phase 2: activates BIDS mode for this 
                          #   (empty) RAVE project or existing BIDS project
                          # phase 3: convert this folder to BIDS-compliant
```

The function should generate `dataset_description.json` and `participants.tsv` if missing. Then create another `dataset_description.json` under `bids_root/derivatives/rave`. 

The description in the `derivatives` folder will include critical RAVE information (such as `"DatasetLinks"`).

```json
{
  ...
  "SourceDatasets": [
    { "URL" : "../.." }   # or "../../rawdata"; see "Source vs. raw vs. derived data"
  ],
  "DatasetLinks" : {
    "bids-root": "../..", # indicate the root of the BIDS dataset directory
    "freesurfer-root": "../../derivatives/freesurfer"  
  }
}
```

This phase also includes re-wiring the RAVE subject directory tree. The goal is to create a directory tree that is both BIDS-compliant and RAVE compatible.

To achieve this goal, we may need to create a "rewiring" table at the subject derivative folder. The format might look like this:

```
{
  # BIDS-URI
  "rave:imaging_path:fs" : "bids:freesurfer-root:freesurfer/sub-<label>",
  
  # No-mapping (this file is original)
  "rave:data_path:power" : ""
  
  # No-mapping (this file is missing, throw error)
  "rave:data_path:phase" : null
}
```

The keys start with `rave:`, followed by keys of `raveio::rave_directories`, and then the relative path. `"rave:imaging_path:fs"` will be the subject `raw_dir/<label>/rave-imaging/fs` path, mapped by BIDS `freesurfer/sub-<label>` folder.

`"rave:data_path:power"` is wired to `""`, which means there is no re-wiring and power path remain unchanged.

`"rave:data_path:phase"` is wired to `null`, meaning the data is missing and will throw error when users try to use it.  

This rewiring look-up table can be used future to indicate not only files within the BIDS but also the remote links (URL, AWS S3, ...)

### Phase 3: Allow RAVE to write to BIDS

Under BIDS mode, the raw data, if not in the BIDS folder will be automatically loaded into the BIDS raw data after processing.

`proj$convert_to_bids()` also converts existing non-BIDS compliant data set into BIDS-compliant one.

`bidsr::bids_validate` function should be implemented (using the official python package).

Implement new `raveio::archive_subject` that allows users to choose a subset of the data that is needed for analysis, and all other files are placeholders.

Data tools will also check the missing file links, and give reports


(what about rebuild the entire RAVE project folder?)
