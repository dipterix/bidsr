

#' @export
bids_filename_definition <- S7::new_class(
  name = "bids_filename_definition",
  package = "bidsr",
  abstract = FALSE,
  parent = bids_object,
  properties = list(
    data_type = bids_data_type,
    suffix = bids_suffix,
    entities = S7::new_property(
      class = S7::class_list,
      validator = function(value) {
        vlen <- length(value)
        if(!vlen) { return() }
        nms <- names(value)

        if(length(nms) != vlen || any(nms == '')) {
          return("Entity list must be named")
        }
        invalid_item_names <- nms[!vapply(nms, function(nm) {
          item <- value[[nm]]
          S7::S7_inherits(item, bids_entity)
        }, FALSE)]
        if(length(invalid_item_names)) {
          return(sprintf(
            "Invalid items: %s. All BIDS registry entries must inherit `bids_entity` class.",
            paste(invalid_item_names, collapse = ", ")
          ))
        }
        return()
      }
    ),
    description = S7::new_property(S7::class_character),
    format = S7::new_property(
      S7::class_character,
      getter = function(self) {
        # https://bids-specification.readthedocs.io/en/stable/common-principles.html#uniqueness-of-data-files
        # Data (not meta) files MUST be uniquely identified by BIDS path components (entities, datatype, suffix)
        #
        # filename registry defines the possible filename so the key is datatype + suffix
        sprintf("%s/%s", self@data_type, self@suffix)
      }
    ),
    print = S7::new_property(
      S7::class_character,
      getter = function(self) {
        descr <- self@description
        if(length(descr)) {
          descr <- sprintf("\n   - %s", paste(descr, collapse = ""))
        } else {
          descr <- ""
        }
        sprintf("BIDS filename definition for `%s`%s", self@format, descr)
      }
    )
  )
)


#' @export
register_bids_filename_definition <- function(definition, overwrite = FALSE) {
  S7::check_is_S7(definition, bids_filename_definition)
  def_key <- tolower(as.character(definition))
  if(bids_entity_table$has(def_key)) {
    if( is.na( overwrite ) ) {
      warning(sprintf(
        "Overwriting existing BIDS filename definition `%s`. If you want to supress this warning, set `overwrite=TRUE`",
        def_key))
      overwrite <- TRUE
    }
    if( !overwrite ) {
      stop(sprintf(
        "BIDS filename definition `%s` already exists. If you want to overwrite this definition, set `overwrite=TRUE`",
        def_key))
    }
  }
  bids_entity_table$set(key = def_key, value = definition)
}

#' @export
get_bids_filename_definition <- function(definition) {
  if(is.character(definition)) {
    def <- bids_entity_table$get(tolower(definition), missing = NULL)
    if(is.null(def)) {
      stop("BIDS filename definition does not exists for: ",
           definition,
           ". If this definition does not exist, you can create your own. ",
           "See `?register_bids_filename_definition` for details")
    }
    definition <- def
  }
  S7::check_is_S7(definition, bids_filename_definition)
  definition
}
