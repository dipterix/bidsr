
#' @export
bids_object <- S7::new_class(
  name = "bids_object",
  package = "bidsr",
  abstract = TRUE
)

#' @export
`format.bidsr::bids_object` <- function(x, ...) {
  S7::check_is_S7(x)
  if( S7::prop_exists(x, "format") ) {
    x@format
  } else {
    character(0L)
  }

}

#' @export
`as.character.bidsr::bids_object` <- function(x, ...) {
  paste(format(x), collapse = "\n")
}

#' @export
`print.bidsr::bids_object` <- function(x, ...) {
  S7::check_is_S7(x)
  if( S7::prop_exists(x, "print") ) {
    cat(paste(c(x@print, ""), collapse = "\n"))
  } else {
    str(x, ...)
  }
  invisible(x)
}

#' @export
`names.bidsr::bids_object` <- function(x) {
  S7::prop_names(x)
}

#' @export
`as.list.bidsr::bids_object` <- function(x, all.names = FALSE, sorted = FALSE, recursive = FALSE, ...) {
  nms <- S7::prop_names(x)
  nms <- nms[!nms %in% c("format", "print")]

  if( !all.names ) {
    nms <- nms[!startsWith(nms, ".")]
  }

  if( sorted ) {
    nms <- sort(nms)
  }

  re <- structure(
    names = nms,
    lapply(nms, function(nm) {
      v <- S7::prop(x, nm)
      if( recursive && S7::S7_inherits(v, bids_object) ) {
        v <- as.list(v, all.names = all.names, sorted = sorted, recursive = recursive)
      }
      v
    })
  )

  if( !all.names ) {
    re <- re[vapply(re, function(item) {
      if(!length(item)) { return(FALSE) }
      TRUE
    }, FALSE)]
  }
  re
}

#' @export
`as.data.frame.bidsr::bids_object` <- function(x, ...) {
  li <- as.list(x, all.names = FALSE, recursive = FALSE, sorted = FALSE)
  as.data.frame(li)
}

#' @export
`$.bidsr::bids_object` <- function(x, name) {
  x[[name]]
}

#' @export
`$<-.bidsr::bids_object` <- function(x, name, value) {
  x[[name]] <- value
}


#' @export
`[[.bidsr::bids_object` <- function(x, name) {
  if(!S7::prop_exists(x, name)) { return(NULL) }
  S7::prop(x, name)
}

#' @export
`[[<-.bidsr::bids_object` <- function(x, name, value) {
  if(!S7::prop_exists(x, name)) {
    cls <- class(x)[[1]]
    stop(sprintf("BIDS class `%s` does not have property `%s`", cls, name))
  }
  S7::prop(x, name) <- value
  x
}
