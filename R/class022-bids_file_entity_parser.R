#' @title Parse 'BIDS' entities from file path
#' @param path path to the entity file, recommended to input the absolute path
#' or relative path from the 'BIDS' root directory
#' @param auto_cache whether to automatically cache the class definition to
#' speed to next time; default is true
#' @param schema_key 'BIDS' schema key if explicit entity rules is needed
#' @param bids_version 'BIDS' version to query the entity rules
#' @returns A \code{'bids_entity_file'} instance.
#' @examples
#'
#'
#' path <- "anat/sub-01_chunk-001_t1w.nii.gz"
#'
#' # --- parse ------------------------------------------------
#' parsed_filename <- parse_path_bids_entity(path)
#' parsed_filename
#'
#' parsed_filename$get_bids_entity("sub")
#'
#' # alternatively
#' parsed_filename$entities$sub$value
#'
#' # data type is `anat` imaging
#' parsed_filename$data_type
#'
#' # data is T1-weighted
#' parsed_filename$suffix
#'
#' # --- usage ------------------------------------------------
#' # use it as character
#' file.path("/path/to/bids/dir/sub-01", parsed_filename)
#'
#' # modify
#' parsed_filename$entities$task <- "special"
#'
#' # new file path: anat/sub-01_task-special_chunk-001_T1w.nii.gz
#' parsed_filename
#'
#' # ---- schema -----------------------------------------------
#' # get BIDS entity rules
#' parsed_filename$get_bids_entity("task")
#'
#' # get entity rules
#' parsed_filename$get_bids_entity_rules()
#'
#'
#' @export
parse_path_bids_entity <- function(path, auto_cache = TRUE, schema_key = NA, bids_version = current_bids_version()) {
  stopifnot(length(path) == 1 && !is.na(path))
  stopifnot(length(schema_key) == 1)

  data_type <- basename(dirname(path))
  if(
    startsWith(data_type, "ses-") ||
    startsWith(data_type, "sub-")
  ) {
    # This is a root file for session/subject
    # e.g. sub-06/ses-ieeg01/sub-06_ses-ieeg01_scans.tsv
    data_type <- "root"
    auto_cache <- FALSE
  }
  file_name <- basename(path)
  parsed <- strsplit(file_name, "_")[[1]]
  # check if connector exists
  parsed_rev <- rev(parsed)
  has_dash_rev <- vapply(parsed_rev, function(xp) {
    grepl("-", xp, fixed = TRUE)
  }, FALSE)

  file_ending <- NULL
  for(ii in seq_along(parsed_rev)) {
    if( ii == 1 || !has_dash_rev[[ii]] ) {
      file_ending <- c(parsed_rev[[ii]], file_ending)
    } else if(has_dash_rev[[ii]]) {
      break
    }
  }
  parsed <- c(
    parsed[seq_len( length(parsed) - length(file_ending) )],
    paste(file_ending, collapse = "_")
  )

  n_parsed <- length(parsed)
  postfix <- parsed[[n_parsed]]

  postfix <- strsplit(postfix, ".", fixed = TRUE)[[1]]
  suffix <- postfix[[1]]
  ext <- paste(postfix[-1], collapse = ".")

  identifier <- tolower(sprintf("%s/%s", data_type, suffix))

  definition <- NULL
  if(!is.na(schema_key)) {
    definition <- bids_entity_file_registry$query_by_schema_key(schema_key)
  }
  if(is.null(definition)) {
    definition <- bids_entity_file_registry$query_by_datatype_suffix(identifier)
  }
  if(is.null(definition)) {

    cls_name <- sprintf("bids_entity_file_%s_%s", data_type, tolower(suffix))
    definition <- new_bids_entity_file_class(
      name = cls_name,
      data_type = data_type,
      suffix = suffix,
      schema_key = schema_key,
      bids_version = bids_version
    )

    if( auto_cache ) {
      # register it
      registry_impl <- bids_entity_file_registry$registry_impl()
      if(!is.na(schema_key)) {
        # definition is defined via `schema_key`
        registry_impl$set(schema_key, definition)
      } else if(data_type != "root") {
        # definition is defined via "identifier"
        registry_impl$set(identifier, definition)
      }
    }
  }

  entities <- parsed[-n_parsed]
  entities <- do.call("rbind", lapply(strsplit(entities, "-", fixed = TRUE), function(x) {
    c(x[[1]], paste(x[-1], collapse = "-"))
  }))

  args <- structure(
    names = entities[,1],
    as.list(entities[, 2])
  )
  args$.extension <- ext
  args$.suffix <- suffix
  args$parent_directory <- dirname(path)
  do.call(definition, args)

}
