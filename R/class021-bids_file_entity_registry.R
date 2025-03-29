# Registry for "active" BIDS version

bids_entity_file_registry <- local({

  # key is datatype/suffix/schema_key
  registry <- NULL
  ensure_registry <- function() {
    if(is.null(registry)) {
      registry <<- fastmap::fastmap(missing_default = NULL)
    }
  }

  clear_registry <- function() {
    ensure_registry()
    registry$reset()
  }

  count <- function() {
    ensure_registry()
    registry$size()
  }

  list_all <- function(sort = TRUE) {
    ensure_registry()
    registry$as_list(sort = sort)
  }

  query_by_datatype_suffix <- function(datatype_suffix) {
    ensure_registry()
    registry$get(datatype_suffix, missing = NULL)
  }
  query_by_schema_key <- function(schema_key) {
    ensure_registry()
    registry$get(schema_key, missing = NULL)
  }
  registry_impl <- function() {
    ensure_registry()
    registry
  }
  list(
    query_by_schema_key = query_by_schema_key,
    query_by_datatype_suffix = query_by_datatype_suffix,
    registry_impl = registry_impl,
    count = count,
    list_all = list_all,
    clear_all = clear_registry
  )
})


# bids_entity_file_registry_get <- function(identifier, ifnotfound = NULL, overwrite = NA) {
#   if(!isFALSE(overwrite) && length(ifnotfound) == 1 && inherits(ifnotfound, "S7_class")) {
#     definition <- ifnotfound
#   } else {
#     definition <- NULL
#   }
#   bids_entity_file_registry$query(identifier = identifier, definition = definition, overwrite = overwrite, ifnotfound = ifnotfound)
# }
#
#
# bids_entity_file_registry_list <- function(data_types = NA) {
#   re <- bids_entity_file_registry$list_all()
#   if(length(data_types) != 1 || !is.na(data_types)) {
#     dtypes <- vapply(strsplit(names(re), split = "/", fixed = TRUE), function(x){x[[1]]}, "")
#     re <- re[dtypes %in% data_types]
#   }
#   re
# }
