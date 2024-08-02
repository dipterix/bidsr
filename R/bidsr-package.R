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

