#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom checkmate check_atomic
#' @importFrom fastmap fastmap
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom utils str
#' @importFrom uuid UUIDgenerate
## usethis namespace: end
NULL

# See https://cran.r-project.org/web/packages/S7/vignettes/packages.html

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

.onLoad <- function(...) {
  S7::methods_register()

  suppressWarnings({
    tryCatch({
      build_default_filename_registry()
    }, error = function(e) {})
  })
}

.onAttach <- function(libname, pkgname) {
  if (getRversion() < "4.3.0") {
    requireNamespace("S7")
  }
}

BIDS_VERSION <- "1.9.0"

# Default BIDS folder depth
BIDS_MAP_MAX_DEPTH <- function() {
  depth <- as.integer(getOption("bidsr.map.seach_depth", Sys.getenv("BIDS_MAP_MAX_DEPTH", "29")))
  if(length(depth) != 1 || is.na(depth) || !is.numeric(depth) || is.infinite(depth)) {
    depth <- 29L
  }  else if (depth < 0) {
    depth <- 0L
  }
  depth
}

DEFAULT_GENERATED_BY <- function() {
  desc <- read.dcf(system.file("DESCRIPTION", package = "bidsr"))
  desc <- structure(names = colnames(desc), as.list(desc))
  bids_dataset_generated_by(
    Name = as.character(desc$Package),
    Version = as.character(desc$Version),
    Description = paste(desc$Description, collapse = "\n"),
    CodeURL = as.character(desc$URL)
  )
}


