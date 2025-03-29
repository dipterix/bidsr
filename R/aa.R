# .function_modifiers <- local({
#
#   raise_errors <- NULL
#
#   set_flag <- function(v) {
#     if(v) {
#       v <- TRUE
#     } else {
#       v <- NULL
#     }
#     raise_errors <<- v
#   }
#
#   check_flag <- function() {
#     if(isTRUE(raise_errors)) {
#       invokeRestart("bids_modifier_condition", raise_errors)
#       stop("Flag is ON, stop evaluating the rests. You should not see this error. Please file a bug report.")
#     }
#     NULL
#   }
#
#   list(
#     set_flag = set_flag,
#     check_flag = check_flag
#   )
#
# })
#
# modify_function <- function(fun) {
#   if(!is.function(fun)) { return(fun) }
#   expr <- body(fun)
#
#   expr_list <- as.list(expr)
#   if(length(expr_list)) {
#     if( identical(expr_list[[1]], as.name("{")) ) {
#       expr_list <- expr_list[-1]
#     }
#   }
#   body(fun) <- as.call(c(list(as.name("{"), quote( asNamespace("bidsr")$.function_modifiers$check_flag() )), expr_list))
#
#   .function_modifiers$set_flag(TRUE)
#   on.exit({
#     .function_modifiers$set_flag(FALSE)
#   })
#
#   withRestarts({
#     fun()
#   }, bids_modifier_condition = function(e) {
#   })
#   .function_modifiers$set_flag(FALSE)
#   fun
# }


key_missing <- structure(list(), class = "key_missing")

blank_named_list <- function() {
  structure(list(), names = character(0L))
}

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

new_function <- function (
    args = alist(), body = {}, env = parent.frame(),
    quoted = FALSE, quasi_env = parent.frame()
) {
  if(!quoted) {
    body <- substitute(body)
  }
  f <- local({ function() {} }, envir = env)
  formals(f) <- args
  body(f) <- body
  f
}

use_docstring <- function(check_param, docstring) {
  tryCatch(
    {
      force(check_param)
    },
    error = function(e) {
      docstring <- trimws(docstring)
      e$message <- paste(c(e$message, "\n\n", docstring), collapse = "\n")
      stop(e)
    }
  )
}
