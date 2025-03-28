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
