
key_missing <- structure(list(), class = "key_missing")
do_nothing <- function(...) {}
missing_property <- function(self) { key_missing }

is_key_missing <- function(x) {
  is.null(x) || identical(x, key_missing)
}


drop_nulls <- function (x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

json_indent <- function() {
  getOption("bidsr.json_indent", 2)
}

# FIXME when S7 bug is fixed
# https://github.com/RConsortium/S7/issues/403
get_prop_in_getter <- function(self, name) {
  attr(self, name, exact = TRUE)
}


# Correct path if prefix has incorrect cases (works on case-sensitive os)
correct_filepath <- function(path) {
  stopifnot(length(path) == 1)
  if(file.exists(path)) { return(path) }
  # path_prefix = "bidsr.rproj"
  dir <- dirname(path)
  prefix <- basename(path)
  files <- list.files(
    path = dir,
    full.names = FALSE,
    all.files = TRUE,
    recursive = FALSE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  sel <- toupper(files) == toupper(prefix)
  if(!any(sel)) { return(NA) }

  return(files[sel][[1]])
}
