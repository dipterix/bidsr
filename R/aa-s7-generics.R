# ---- S3-S7 common -------------------------------------------------------

format.generic <- S7::new_external_generic("base", "format", "x")
as.character.generic <- S7::new_external_generic("base", "as.character", "x")
print.generic <- S7::new_external_generic("base", "print", "x")
names.generic <- S7::new_external_generic("base", "names", "x")
as.list.generic <- S7::new_external_generic("base", "as.list", "x")
as.data.frame.generic <- S7::new_external_generic("base", "as.data.frame", "x")

extract_bracket.generic <- S7::new_external_generic("base", "[[",  c("x", "name"))
# extract_set_bracket.generic <- S7::new_external_generic("base", "[[<-", c("x", "name", "value"))

extract.generic <- S7::new_external_generic("base", "$", c("x", "name"))
extract_set.generic <- S7::new_external_generic("base", "$<-", c("x", "name", "value"))


# ---- Entity -------------------------------------------------------
#' @title Get 'BIDS' entity values from file
#' @param x 'BIDS' file path or parsed object; see 'Examples'
#' @param key entity key
#' @param value_only whether to return the value only; default is true; set
#' to \code{FALSE} to return the entity object
#' @param ifnotfound default value to return is the entity is missing
#' @returns 'BIDS' entity value or object, depending on \code{value_only}
#' @examples
#'
#' # Quick usage
#' get_bids_entity("sub-YAB_ses-01_task-AV_ieeg.mat", "sub")
#'
#' get_bids_entity_rules("ieeg/sub-YAB_ses-01_task-AV_channels.tsv")
#'
#' # Full usage
#' parsed <- parse_path_bids_entity(
#'     path = "ieeg/sub-YAB_ses-01_task-AV_channels.tsv")
#'
#' parsed$get_bids_entity("sub")
#' parsed$get_bids_entity_rules()
#'
#' parsed$description
#' parsed$entities
#'
#' @export
get_bids_entity <- S7::new_generic("get_bids_entity", "x", function(x, key, value_only = TRUE, ifnotfound = NULL) {
  force(key)
  S7::S7_dispatch()
})

S7::method(get_bids_entity, S7::class_character) <- function(x, key, value_only = TRUE, ifnotfound = NULL) {
  force(key)
  parsed <- parse_path_bids_entity(path = x)
  get_bids_entity(x = parsed, key = key, value_only = value_only, ifnotfound = ifnotfound)
}

#' @rdname get_bids_entity
#' @export
get_bids_entity_rules <- S7::new_generic("get_bids_entity_rules", "x", function(x) {
  S7::S7_dispatch()
})

S7::method(get_bids_entity_rules, S7::class_character) <- function(x) {
  parsed <- parse_path_bids_entity(path = x)
  get_bids_entity_rules(x = parsed)
}


# ---- Sidecars -------------------------------------------------------
#' @rdname bids_tabular
#' @export
as_bids_tabular <- S7::new_generic("as_bids_tabular", "x")

#' @rdname bids_tabular
#' @export
save_bids_tabular <- S7::new_generic("save_bids_tabular", "x", function(x, path, meta = TRUE, ...) {
  S7::S7_dispatch()
})

#' @rdname bids_dataset_description
#' @export
as_bids_dataset_description <- S7::new_generic(
  name = "as_bids_dataset_description",
  dispatch_args = "x",
  fun = function(x, parent_directory, ...) {
    S7::S7_dispatch()
  }
)
