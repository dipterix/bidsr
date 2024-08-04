# Register official registries
# https://bids-specification.readthedocs.io/en/stable/appendices/entity-table.html

bids_entity_table <- fastmap::fastmap()

build_bids_filename_registry <- function(
    definition_table, entity_table,
    # glossary
    data_type_table = NULL, suffix_table = NULL,
    overwrite = FALSE) {

  if(is.null(data_type_table)) {
    data_type_table <- system.file("definitions", "glossary", "DataType.csv", package = "bidsr")
  }
  if(is.null(suffix_table)) {
    suffix_table <- system.file("definitions", "glossary", "Suffix.csv", package = "bidsr")
  }

  # DIPSAUS DEBUG START
  # definition_table <- system.file("definitions", "MagneticResonanceImaging.csv", package = "bidsr")
  # entity_table <- system.file("definitions", "MagneticResonanceImagingEntity.csv", package = "bidsr")
  # data_type_table <- system.file("definitions", "glossary", "DataType.csv", package = "bidsr")
  # suffix_table <- system.file("definitions", "glossary", "Suffix.csv", package = "bidsr")
  # ii=1

  load_csv_if_is_path <- function(x) {
    if(is.data.frame(x)) { return(x) }
    if(!file_exists(x)) {
      stop("Path `", x, "` does not exists")
    }
    suppressWarnings({ utils::read.csv(x) })
  }
  definition_table <- load_csv_if_is_path(definition_table)
  entity_table <- load_csv_if_is_path(entity_table)
  data_type_table <- load_csv_if_is_path(data_type_table)
  suffix_table <- load_csv_if_is_path(suffix_table)

  definition_table$DataType <- tolower(definition_table$DataType)
  data_type_table$Name <- tolower(data_type_table$Name)
  entity_table$Name <- tolower(entity_table$Name)
  entity_table$Type <- tolower(entity_table$Type)

  def_entities <- names(definition_table)
  def_entities <- def_entities[!def_entities %in% c("DataType", "Suffix")]

  entity_storage_mode <- structure(
    names = entity_table$Name,
    as.list(entity_table$Type)
  )
  if(!all(def_entities %in% entity_table$Name)) {
    missing_entities <- def_entities[!def_entities %in% entity_table$Name]
    stop("The following entity storage types are missing: ", paste(missing_entities, collapse = ", "))
  }

  data_type_descriptions <- structure(
    names = data_type_table$Name,
    as.list(data_type_table$FullName)
  )


  # suffix_table$Name <- tolower(suffix_table$Name)

  lapply(seq_len(nrow(definition_table)), function(ii) {
    item <- definition_table[ii, ]

    entities <- structure(
      names = def_entities,
      lapply(def_entities, function(entity_key) {
        bids_entity(key = entity_key,
                    requirement = item[[entity_key]],
                    type = entity_storage_mode[[entity_key]])
      })
    )

    descr <- trimws(data_type_descriptions[[item$DataType]])
    if(length(descr) != 1 || is.na(descr) || !nzchar(descr)) {
      descr <- character(0L)
    }

    def <- bids_filename_definition(
      data_type = item$DataType,
      suffix = item$Suffix,
      entities = entities,
      description = descr
    )

    register_bids_filename_definition(definition = def, overwrite = overwrite)
  })

}


build_default_filename_registry <- function() {
  registry_dir <- system.file("definitions", package = "bidsr")
  registries <- list.files(
    registry_dir,
    pattern = "csv$",
    full.names = FALSE,
    recursive = FALSE,
    ignore.case = TRUE
  )

  prefix <- unique(gsub("(Entity\\.csv|\\.csv)$", "", registries))

  lapply(prefix, function(p) {
    def_path <- file.path(registry_dir, sprintf("%s.csv", p))
    ent_path <- file.path(registry_dir, sprintf("%sEntity.csv", p))
    if(file_exists(def_path) && file_exists(ent_path)) {
      build_bids_filename_registry(
        definition_table = def_path,
        entity_table = ent_path,
        overwrite = TRUE
      )
    }
  })

}
