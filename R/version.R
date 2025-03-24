# read BIDS schema

bids_versions <- local({
  versions <- NULL
  function() {
    if(!length(versions)) {
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
      versions <<- v
    }
    versions
  }
})

use_bids_version <- function(version = bids_versions()) {
  version <- match.arg(version)
  options("bidsr.bids_version" = version)
  invisible(version)
}

current_bids_version <- function() {
  ver <- getOption("bidsr.bids_version", default = NA)
  if(is.na(ver)) {
    ver <- bids_versions()[[1]]
    options("bidsr.bids_version" = ver)
  }
  ver
}


