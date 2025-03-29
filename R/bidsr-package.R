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

.onLoad <- function(libname, pkgname) {

  S7::methods_register()

  current_ver <- as.character(Sys.getenv("BIDSR_BIDS_VERSION", unset = ""))
  if( length(current_ver) == 1 && !is.na(current_ver) && nzchar(current_ver) ) {
    suppressWarnings({
      tryCatch({
        use_bids_version(current_ver)
      }, error = function(e) {})
    })
  }
}

.onAttach <- function(libname, pkgname) {
  if (getRversion() < "4.3.0") {
    requireNamespace("S7")
  }
}



