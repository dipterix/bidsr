# Register official registries
# https://bids-specification.readthedocs.io/en/stable/appendices/entity-table.html

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

  data_type_table <- utils::read.csv(system.file("definitions", "glossary", "DataType.csv", package = "bidsr"))
  suffix_table <- utils::read.csv(system.file("definitions", "glossary", "Suffix.csv", package = "bidsr"))

  lapply(prefix, function(p) {
    definition_table <- file.path(registry_dir, sprintf("%s.csv", p))
    entity_table <- file.path(registry_dir, sprintf("%sEntity.csv", p))

    register_bids_file_entiries(
      definition_table = definition_table,
      entity_table = entity_table,
      data_type_table = data_type_table,
      suffix_table = suffix_table,
      overwrite = NA
    )
  })

}
