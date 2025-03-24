# DIPSAUS DEBUG START
# schema_raw <- load_schema_file()
# schema <- bids_schema()
# schema$objects$columns
# bids_version = current_bids_version()
# file_schema_key <- "rules.files.raw.anat.parametric"

# li <- as.list(schema)
#
# schema_keys <- names(unlist(li))
# schema_keys <- schema_keys[startsWith(schema_keys, "rule")]
# schema_keys <- unique(schema_keys)
# schema_keys <- schema_keys[grepl("\\.(suffixes|extensions|datatypes|entities)", schema_keys)]
#
# m <- stringr::str_split(schema_keys, pattern = "\\.", simplify = TRUE, n = 7)
# m <- m[grepl("(suffixes|extensions|datatypes|entities)", m[,6]), ]
#
# all_keys <- unique(apply(m[, 1:5], 1, paste, sep = ".", collapse = "."))

get_schema_entity_order <- function(bids_version = current_bids_version()) {
  schema <- load_schema_file(bids_version = bids_version)
  schema$rules$entities
}

get_schema_entity_rule <- function(entity_schema_name, bids_version = current_bids_version()) {
  # entity_name (e.g. subject), not key (sub)
  schema <- load_schema_file(bids_version = bids_version)

  entity_schema <- schema$objects$entities[[entity_schema_name]]
  if(!length(entity_schema) || length(entity_schema$name) != 1) { return(NULL) }

  entity_schema
}

get_schema_file_entity_rules <- function(file_schema_key, bids_version = current_bids_version()) {
  # schema <- load_schema_file(bids_version = bids_version)
  schema_definition <- get_bids_schema(keys = file_schema_key[[1]], bids_version = bids_version)


  if(!length(schema_definition)) { return(NULL) }

  if(!length(schema_definition$suffixes)) { return(NULL) }
  if(!length(schema_definition$datatypes)) { return(NULL) }
  # extensions <- schema_definition$extensions
  # datatypes <- schema_definition$datatypes

  # entities
  entities <- schema_definition$entities
  entity_names <- names(entities)
  entity_rules <- list()

  if(length(entity_names)) {
    # re-order
    key_entities <- get_schema_entity_order(bids_version)
    key_entities <- key_entities[key_entities %in% entity_names]
    rest_entities <- setdiff(entity_names, key_entities)
    entity_names <- c(key_entities, rest_entities)

    entity_schemas <- drop_nulls(lapply(entity_names, function(name) {
      entity_schema <- get_schema_entity_rule(name, bids_version = bids_version)
      if(!length(entity_schema) || length(entity_schema$name) != 1) { return(NULL) }
      entity_schema$required <- entities[[name]]
      entity_schema
    }))

    if(length(entity_schemas)) {
      entity_keys <- unlist(lapply(entity_schemas, "[[", "name"))
      entity_rules <- structure(
        names = entity_keys,
        lapply(entity_schemas, function(x) {
          c(x$format, x$required)
        })
      )
    }
  }

  schema_definition$entities <- entity_rules

  return(schema_definition)
}







































# get_bids_schema("rules.tabular_data.modality_agnostic.Participants")
# get_bids_schema("rules.tabular_data.modality_agnostic.Participants")
# name__channels
# schema$objects$columns$name__channels
#
#
# # https://bidsschematools.readthedocs.io/en/latest/description.html#filename-construction-rules
# # https://bidsschematools.readthedocs.io/en/latest/description.html#sidecar-and-tabular-data-rules
#
# schema$rules$tabular_data$ieeg$iEEGChannels$initial_columns
#               extension   modality/datatype  suffix/filetype
#
#
# schema$rules$sidecars$anat$MRIAnatomyCommonMetadataFields$fields
#
# schema$rules$dataset_metadata$dataset_description$fields$BIDSVersion
#
# names(schema$rules)
# schema$rules$files$common$core$dataset_description
# schema$rules$files$raw$anat$mp2rage
#
#
# "dataset_metadata"  "directories"       "entities"
# [6] "errors"            "files"             "json"              "modalities"        "sidecars"
# [11] "tabular_data"
#
#
# schema$rules$common_principles
# schema$rules$directories$raw$subject$entity
# schema$objects$entities$subject
#
# schema$rules$entities
# schema$objects$common_principles$data_acquisition
#
#
# schema$rules$files$common$tables$scans
# schema$rules$files$raw$eeg
# schema$rules$json$ieeg$iEEGCoordsystemGeneral
# schema$rules$modalities$mri
#
# schema$rules$common_principles
