#' @title Internal development memo
#' @author Zhengjia Wang
#'
#' @section File-naming Convention:
#'
#' The file name convention helps me quickly index the class definitions.
#' There are 5 tiers of class files:
#'    class00x: (schema and basic definitions)
#'            001: constants
#'            002: Abstract base classes (`BIDSClassBase`), generics, methods
#'            003: properties (`bids_property_*`), for convenient `S7` class
#'                property reuses
#'            004: schema parsing
#'            005: context
#'            006-009: reserved for schema instantiation
#'    class01x: Basic data class
#'            011: Map class (??)
#'            012: entity
#'    class02x: (file-system, hierarchy)
#'            020: file-name with BIDS entity parsing
#'            021: directory inheritance rules
#'    class03x: (modality agnostic files)
#'            030: dataset_description.json
#'            031: participants.tsv
#'                 participants.json
#'            032: samples.tsv
#'                 samples.json
#'            033: Phenotypic and assessment data
#'                      phenotype/
#'                        <measurement_tool_name>.tsv
#'                        <measurement_tool_name>.json
#'            034: Scans file
#'                      sub-<label>/
#'                        [ses-<label>/]
#'                          sub-<label>[_ses-<label>]_scans.tsv
#'                          sub-<label>[_ses-<label>]_scans.json
#'            035: Sessions file
#'                      sub-<label>/
#'                        sub-<label>_sessions.tsv
#'            038: Project & subject
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


