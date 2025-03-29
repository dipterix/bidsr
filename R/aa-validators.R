union_validators <- function(..., .list = list()) {
  dot_len <- ...length()
  list_len <- length(.list)

  total_len <- dot_len + list_len
  if( total_len == 0L ) { return(NULL) }


  if( total_len == 1L ) {
    if(dot_len == 1L) {
      return(...elt(1))
    } else {
      return(.list[[1]])
    }
  }

  validators <- drop_nulls(c(list(...), .list))
  if(length(validators) == 1) {
    return(validators[[1]])
  }

  function(value) {
    for(is_valid in validators) {
      if(is.function(is_valid)) {
        re <- is_valid(value)
        if(!is.null(re) && !isTRUE(re)) {
          return(re)
        }
      }
    }
    return(NULL)
  }
}

validator_must_missing <- function(value) {
  if(length(value) == 0) {
    return(NULL)
  }
  "value must NOT be specified"
}
validator_must_not_missing <- function(value) {
  if(length(value) == 0) {
    return("value is required")
  }
  NULL
}

validator_wizard <- function(type, ...) {
  checkmate <- asNamespace("checkmate")
  check <- checkmate[[sprintf("check_%s", type)]]
  if(!is.function(check)) {
    stop("Function checkmate::check_%s does not exists.", type)
  }
  args <- list(x = NULL, ...)
  function(value) {
    if(!is.null(value)) {
      args$x <- value
    }
    re <- do.call(check, args)
    if(isTRUE(re)) { return(NULL) }
    re
  }
}

validator_named_list <- function(value) {
  vlen <- length(value)
  if(!vlen) { return() }
  nms <- names(value)

  if(length(nms) != vlen || any(nms == '')) {
    return("list must be named")
  }

  return()
}

validator_unnamed_list <- function(value) {
  nms <- names(value)
  if(length(nms)) {
    return("list must be unnamed")
  }
  return()
}

validator_nonempty_string <- validator_wizard(
  type = "string",
  na.ok = FALSE,
  min.chars = 1,
  null.ok = FALSE
)

validator_nonnegative_intergerish <- function(value) {
  if(!is.numeric(value)) {
    value <- as.numeric(value)
  }
  validator_wizard(
    type = "integerish",
    any.missing = FALSE,
    lower = 0,
    null.ok = FALSE
  )(value)
}

bids_validator_warn <- function(...) {
  s <- c(...)
  if(!inherits(s, "condition")) {
    s <- paste(s, collapse = "")
  }

  if(getOption("bidsr.error_on_warn", FALSE)) {
    stop(s)
  }
  if(!getOption("bidsr.muffle_warnings", FALSE)) {
    warning(s)
  }
  invisible(s)
}
