
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

