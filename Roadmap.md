## Road-maps

Currently heading towards milestone: 2

### Milestone 1: Define BIDS classes

* Base classes
  - [X] R `S7` base classes and properties
  - [X] tabular data
  - [X] `JSON` side-car
  - [X] `BIDS` entity
  - [X] `BIDS` entity collection
* Class definitions for common files
  - Modality agnostic files:
    * [X] `dataset_description.json`
    * [X] `README`
    * [ ] ~~`CITATION.cff`, `CHANGES`, `LICENSE`~~
    * [X] `participants.tsv`, `participants.json`
    * [X] `samples.tsv`, `samples.json`
    * [X] `phenotype/`
    * [X] Scans file
    * [X] Sessions file
  - Modality specific files:
    * [X] Query files by data type for each subject
  
### Milestone 2: Comprehensive file look-up/query

* Provide read functions to handle common file formats (e.g. `bids_handle_file`)
  - [X] `JSON` format
  - [X] `.tsv` or `.csv` format
  - [ ] `.nii`, `.gii`
  - [ ] `.mat`, `.edf`, ... (electrophysiology)
* Query `BIDS` files
  - [X] Get top-level files
  - [X] Analyze and list subject data by types
  - [X] resolve basic BIDS URI
  - [ ] Query project, find side-car and meta-data information using the `BIDS` inheritance principle
* Schema
  - [X] Parse BIDS schema automatically
  - [X] Use `BIDS` schema to generate file entity rules dynamically

### Milestone 3: Dynamic file handlers

* [ ] Provide handler registry to access and process data files
* [ ] Incorporate package `ieegio` to provide default file handlers
* [ ] incorporate project RAVE and rewire the paths to BIDS paths)

### Milestone 4: A BIDS converter

### Milestone 5: BIDS validator


