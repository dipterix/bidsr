#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

# See https://cran.r-project.org/web/packages/S7/vignettes/packages.html

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

.onLoad <- function(...) {
  S7::methods_register()

  build_default_filename_registry()
}

.onAttach <- function(libname, pkgname) {
  if (getRversion() < "4.3.0") {
    requireNamespace("S7")
  }
}

BIDS_VERSION <- "1.9.0"

# Default BIDS folder depth
BIDS_MAP_MAX_DEPTH <- 29L

DEFAULT_GENERATED_BY <- function() {
  desc <- read.dcf(system.file("DESCRIPTION", package = "bidsr"))
  desc <- structure(names = colnames(desc), as.list(desc))
  bids_generated_by(
    Name = as.character(desc$Package),
    Version = as.character(desc$Version),
    Description = paste(desc$Description, collapse = "\n"),
    CodeURL = as.character(desc$URL)
  )
}


