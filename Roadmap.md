# NA

## Road-maps

Currently heading towards milestone: 2

### Milestone 1: Define BIDS classes

Base classes

R `S7` base classes and properties

tabular data

`JSON` side-car

`BIDS` entity

`BIDS` entity collection

Class definitions for common files

Modality agnostic files:

`dataset_description.json`

`README`

~~`CITATION.cff`, `CHANGES`, `LICENSE`~~

`participants.tsv`, `participants.json`

`samples.tsv`, `samples.json`

`phenotype/`

Scans file

Sessions file

Modality specific files:

Query files by data type for each subject

### Milestone 2: Comprehensive file look-up/query

Provide read functions to handle common file formats
(e.g. `bids_handle_file`)

`JSON` format

`.tsv` or `.csv` format

`.nii`, `.gii`

`.mat`, `.edf`, … (electrophysiology)

Query `BIDS` files

Get top-level files

Analyze and list subject data by types

resolve basic BIDS URI

Query project, find side-car and meta-data information using the `BIDS`
inheritance principle

Schema

Parse BIDS schema automatically

Use `BIDS` schema to generate file entity rules dynamically

### Milestone 3: Dynamic file handlers

Provide handler registry to access and process data files

Incorporate package `ieegio` to provide default file handlers

incorporate project RAVE and rewire the paths to BIDS paths)

### Milestone 4: A BIDS converter

### Milestone 5: BIDS validator
