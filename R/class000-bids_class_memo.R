#' @title Internal development memo
#' @author Zhengjia Wang
#'
#' @section File-naming Convention:
#'
#' The file name convention helps me quickly index the class definitions.
#' There are 5 tiers of class files:
#'    class00x: (basic definitions)
#'            001: Base classes (`bids_class_base`), generics, methods
#'            002: properties (`bids_property_*`), for convenient `S7` class
#'                property reuses
#'            003-009: data structures such as `bids_map`, `bids_uri`,
#'                `bids_tabular`, ...
#'    class01x: (file-system, hierarchy)
#'            010: file-name with BIDS entity parsing
#'            011: directory inheritance rules
#'    class02x: (modality agnostic files)
#'            020: dataset_description.json
#'            021: participants.tsv
#'                 participants.json
#'            022: samples.tsv
#'                 samples.json
#'            023: Phenotypic and assessment data
#'                      phenotype/
#'                        <measurement_tool_name>.tsv
#'                        <measurement_tool_name>.json
#'            024: Scans file
#'                      sub-<label>/
#'                        [ses-<label>/]
#'                          sub-<label>[_ses-<label>]_scans.tsv
#'                          sub-<label>[_ses-<label>]_scans.json
#'            025: Sessions file
#'                      sub-<label>/
#'                        sub-<label>_sessions.tsv
#'    class100-class299: (modality specific files)
#'      Source: https://bids-specification.readthedocs.io/en/stable/,
#'      see Section "Modality Specific Files"
#'      Intracranial Electroencephalography
#'            class10x: Magnetic Resonance Imaging
#'            class11x: Magnetoencephalography
#'            class12x: Electroencephalography
#'            class13x: Intracranial Electroencephalography
#'            class14x: Task events
#'            class15x: Physiological recordings
#'            class16x: Behavioral experiments (with no neural recordings)
#'            class17x: Genetic Descriptor
#'            class18x: Positron Emission Tomography
#'            class19x: Microscopy
#'            class20x: Near-Infrared Spectroscopy
#'            class21x: Motion
#'            class22x: Magnetic Resonance Spectroscopy
#'    class300-class499: (reserved or future extensions)
#'    class500-class999: (derivatives)
#'
#' @noRd
#' @noMd
NULL
