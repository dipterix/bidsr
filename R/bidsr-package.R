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

  ns <- asNamespace(pkgname)
  fun_names <- ls(ns)

  lapply(fun_names, function(nm) {
    fun <- ns[[nm]]
    if(is.function(fun)) {
      ns[[nm]] <- utils::removeSource(fun)
    }
  })

  S7::methods_register()

  suppressWarnings({
    tryCatch({
      # build_default_filename_registry()
    }, error = function(e) {})
  })
}

.onAttach <- function(libname, pkgname) {
  if (getRversion() < "4.3.0") {
    requireNamespace("S7")
  }
}



