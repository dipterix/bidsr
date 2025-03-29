
.bids_schema_loader <- local({

  bids_versions <- NULL
  schema_list <- NULL
  initialized <- FALSE

  ensure_globals <- function() {
    if(!length(bids_versions)) {
      files <- sort(list.files(
        system.file("bids-schema", package = "bidsr"),
        pattern = "schema-.*\\.json",
        recursive = FALSE,
        full.names = FALSE,
        ignore.case = TRUE
      ),
      decreasing = TRUE)
      v <- gsub("^schema-", "", files, ignore.case = TRUE)
      v <- gsub(".json", "", v, ignore.case = TRUE)
      bids_versions <<- v
    }
    if(is.null(schema_list)) {
      schema_list <<- fastmap::fastmap()
    }
    initialized <<- TRUE
  }

  get_bids_versions <- function() {
    bids_versions
  }

  use_bids_version <- function(version = NULL) {
    if(!initialized) { ensure_globals() }
    version <- match.arg(version, choices = bids_versions)
    Sys.setenv("BIDSR_BIDS_VERSION" = version)
    invisible(version)
  }

  current_bids_version <- function() {
    if(!initialized) { ensure_globals() }
    ver <- Sys.getenv("BIDSR_BIDS_VERSION", unset = "")
    if(!isTRUE(ver %in% bids_versions)) {
      ver <- bids_versions[[1]]
      use_bids_version(ver)
    }
    ver
  }


  get_schema <- function(bids_version = current_bids_version()) {
    if(!initialized) { ensure_globals() }

    bids_version <- match.arg(bids_version, bids_versions)

    if(!schema_list$has(bids_version)) {
      schema <- compile_schema(bids_version = bids_version)
      schema_list$set(bids_version, schema)
    } else {
      schema <- schema_list$get(bids_version)
    }

    schema
  }

  list(
    get_bids_versions = get_bids_versions,
    use_bids_version = use_bids_version,
    current_bids_version = current_bids_version,
    get_schema = get_schema
  )

})

bids_versions <- .bids_schema_loader$bids_versions
use_bids_version <- .bids_schema_loader$use_bids_version
current_bids_version <- .bids_schema_loader$current_bids_version
bids_schema <- .bids_schema_loader$get_schema



