#' Base abstract class
#' @description
#' All \code{bidsr} classes should inherit this abstract class, which provides
#' consistent \code{S3} generics. This is an abstract class, which should not
#' be called directly; use \code{\link{new_bids_class}} to create new class
#' definitions.
#' @export
bids_class_base <- S7::new_class(
  name = "bids_class_base",
  package = "bidsr",
  abstract = TRUE
)

class(bids_class_base) <- c("bids_class", class(bids_class_base))

#' Create new \code{bidsr} class definition
#' @description
#' By default, all generated classes inherit \code{\link{bids_class_base}},
#' which provides \code{S3} generics
#' @param name string, required, name of the class
#' @param properties a named list where the names are the property names
#' that can be queried via \code{`$`} or \code{`@`} operators
#' @param methods read-only methods for the class, such as \code{format}
#' and \code{print}; if a method is a function, then the arguments
#' should start with \code{self} (instance method) or \code{cls} (class method).
#' In most of the cases, changes made to the object will not be carrier out
#' once the the method function exits. For changes to the properties,
#' use \code{setter} functions in each property.
#' @param constructor function to custom the constructor; see parameter
#' \code{'constructor'} at \code{\link[S7]{new_class}} for details. Basically
#' A custom constructor should call
#' \code{\link[S7:new_object]{S7::new_object()}} to
#' create the \code{'S7'} object. The first argument should be an instance
#' of the parent class (if used). The subsequent arguments are used to set
#' the properties.
#' @param validator validate function; see \code{\link[S7]{new_class}}
#' @param parent parent class definition, needs to be a \code{'S7'} class
#' @param abstract whether the class is abstract (\code{TRUE}) or not
#' (\code{FALSE})
#' @param hidden_names vector of string, names of properties and/or methods
#' whose presence should be hidden from the users; this will affect \code{`$`}
#' operator, or \code{\link{names}} function. The hidden properties or methods
#' cannot be queried via these two ways. However, properties can still be
#' accessible via \code{`@`} operator
#' @examples
#'
#'
#' # ---- hide properties and attributes -------------------------
#' cls <- new_bids_class(
#'   name = "my_class",
#'   properties = list(
#'     str = bids_property_character(
#'       name = "str", type = "required"),
#'     hidden_prop = bids_property_character("hidden_prop")
#'   ),
#'   methods = list(
#'     # read-only methods
#'     format = function(self, ...) {
#'       sprintf("my_class@str -> %s", self$str)
#'     },
#'     hidden_method = function(self) {
#'       "Nonononono"
#'     }
#'   ),
#'   hidden_names = c("hidden_method", "hidden_prop")
#' )
#'
#' x <- cls(str = "a")
#' x
#'
#' # hidden names will not be displayed
#' names(x)
#' as.list(x)
#'
#' # however, then can still be queried
#' x$hidden_prop
#' x$hidden_method()
#'
#'
#'
#'
#' @export
new_bids_class <- function(
    name,
    parent = bids_class_base,
    abstract = FALSE,
    hidden_names = NULL,
    properties = NULL,
    methods = NULL,
    validator = NULL,
    constructor = NULL
) {

  v <- validator
  # union_validators(
  #   function(self) {
  #     S7::super(self, to = bids_class_base)
  #   },
  #   validator
  # )

  parent_methods <- as.list(attr(parent, ".bids_object_extra"))
  methods <- as.list(methods)
  parent_method_names <- names(parent_methods)[!names(parent_methods) %in% c(names(methods), "")]
  if(length(parent_method_names)) {
    methods[parent_method_names] <- parent_methods[parent_method_names]
  }
  check <- validator_named_list(methods)
  if(!is.null(check)) {
    stop("`new_bids_class`: `methods` must be a named list.")
  }

  methods <- structure(
    names = names(methods),
    lapply(names(methods), function(nm) {
      f <- methods[[nm]]
      if(!is.function(f)) { return( f ) }
      fnames <- names(formals(f))
      if(!length(fnames)) {
        stop(sprintf(sprintf(
          "Method `%s` must start with argument `self` or `cls`", nm
        )))
      }
      if( identical(fnames[[1]], "self") ) {
        return(f)
      } else if( identical(fnames[[1]], "cls") ) {
        return(f)
      } else {
        stop(sprintf(sprintf(
          "Method `%s` must start with argument `self` or `cls`", nm
        )))
      }
    })
  )

  parent_hidden_names <- attr(parent, ".bids_object_hidden_names")
  hidden_names <- unique(c(parent_hidden_names, hidden_names))

  properties <- as.list(properties)

  # method_names <- c(names(properties), names(methods))
  # if("format" %in% method_names && "format" %in% method_names ) {
  #   bids_property_print_format
  # }

  # Make the first dummy class with no constructor altered
  cls <- S7::new_class(
    name = name,
    package = "bidsr",
    abstract = abstract,
    validator = v,
    parent = parent,
    properties = as.list(properties),
    constructor = constructor
  )

  constructor <- attr(cls, "constructor")
  expr <- body(cls)
  expr <- bquote({

    .object <- .(expr)
    attr(.object, ".bids_object_extra") <- .(methods)
    attr(.object, ".bids_object_hidden_names") <- .(hidden_names)

    .object
  })

  body(constructor) <- expr

  # re-run with new constructor
  cls <- S7::new_class(
    name = name,
    package = "bidsr",
    abstract = abstract,
    validator = v,
    parent = parent,
    properties = as.list(properties),
    constructor = constructor
  )

  attr(cls, ".bids_object_extra") <- methods
  attr(cls, ".bids_object_hidden_names") <- hidden_names

  cls_parent <- class(parent)
  cls_parent <- cls_parent[!cls_parent %in% c("bids_class", "S7_class", "S7_object")]
  class(cls) <- unique(c(sprintf("%s_definition", name), cls_parent, "bids_class", class(cls)))

  cls

}

#' @export
`format.bidsr::bids_class_base` <- function(x, indent = json_indent(), collapse = "\n", ...) {
  S7::check_is_S7(x)
  s <- character(0L)
  if( S7::prop_exists(x, "format") ) {
    s <- paste(x@format, collapse = collapse)
  } else {
    fun <- attr(x, ".bids_object_extra")$format
    if(is.function(fun)) {
      if(names(formals(fun))[[1]] == "cls") {
        s <- fun(cls = S7::S7_class(x), indent = indent, ...)
      } else {
        s <- fun(self = x, indent = indent, ...)
      }
    }
  }
  s <- paste(s, collapse = collapse)
  if(!nzchar(s)) { return(character(0L)) }
  if(indent > 0) {
    prefix <- paste(rep(" ", indent), collapse = "")
    s <- paste(sprintf("%s%s", prefix, strsplit(s, "\n")[[1]]), collapse = collapse)
  }
  s
}

#' @export
`as.character.bidsr::bids_class_base` <- function(x, indent = 0, ...) {
  paste(format(x, indent = indent, ...), collapse = "\n")
}

#' @export
`print.bidsr::bids_class_base` <- function(x, ...) {
  S7::check_is_S7(x)
  if( S7::prop_exists(x, "print") ) {
    cat(paste(c(x@print, ""), collapse = "\n"))
  } else if( is.function(attr(x, ".bids_object_extra")$print) ) {
    fun <- attr(x, ".bids_object_extra")$print
    fun(x, ...)
  } else if (S7::prop_exists(x, "format") || is.function(attr(x, ".bids_object_extra")$format)) {
    cat(format(x), sep = "\n")
  } else {
    utils::str(x, ...)
  }
  invisible(x)
}

names.generic <- S7::new_external_generic("base", "names", "x")
S7::method(names.generic, bids_class_base) <- function(x) {
  nms <- unique(c(S7::prop_names(x), names(attr(x, ".bids_object_extra"))))
  nms <- nms[!nms %in% attr(x, ".bids_object_hidden_names")]
  nms
}

as.list.generic <- S7::new_external_generic("base", "as.list", "x")

S7::method(as.list.generic, bids_class_base) <- function(x, all.names = FALSE, sorted = FALSE, recursive = FALSE, ...) {
  nms <- S7::prop_names(x)
  nms <- nms[!nms %in% c("format", "print", attr(x, ".bids_object_hidden_names"))]

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
      if( recursive ) {
        if( S7::S7_inherits(v, bids_class_base) ) {
          v <- as.list(v, all.names = all.names, sorted = sorted, recursive = TRUE)
        } else if( is.list(v) && !is.data.frame(v) ) {
          v <- structure(
            names = names(v),
            lapply(v, function(vi) {
              if( S7::S7_inherits(vi, bids_class_base) ) {
                vi <- as.list(vi, all.names = all.names, sorted = sorted, recursive = TRUE)
              }
              vi
            })
          )
        }

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
`as.data.frame.bidsr::bids_class_base` <- function(x, ...) {
  li <- as.list(x, all.names = FALSE, recursive = FALSE, sorted = FALSE)
  as.data.frame(li)
}

#' @export
`$.bidsr::bids_class_base` <- function(x, name) {
  x[[name]]
}

#' @export
`$<-.bidsr::bids_class_base` <- function(x, name, value) {
  x[[name]] <- value
  x
}


#' @export
`[[.bidsr::bids_class_base` <- function(x, name) {
  if(S7::prop_exists(x, name)) {
    return(S7::prop(x, name))
  }
  # methods
  extra_properties <- as.list(attr(x, ".bids_object_extra"))
  re <- extra_properties[[ name ]]

  if(is.function(re)) {
    fmls <- formals(re)
    first_formal <- names(fmls)[[1]]
    fmls <- fmls[-1]
    if(first_formal == "self") {
      fun <- function(...) {
        call <- match.call()
        call[[1]] <- re
        call$self <- x
        env <- parent.frame()
        eval(call, env)
      }
      formals(fun) <- fmls
      return(fun)
    } else {
      fun <- function(...) {
        call <- match.call()
        call[[1]] <- re
        call$cls <- S7::S7_class(x)
        env <- parent.frame()
        eval(call, env)
      }
      formals(fun) <- fmls
      return(fun)
    }
  }

  return(re)
}

#' @export
`[[<-.bidsr::bids_class_base` <- function(x, name, value) {
  if(!S7::prop_exists(x, name)) {
    cls <- class(x)[[1]]
    if(name %in% names(attr(x, ".bids_object_extra"))) {
      stop(sprintf("BIDS class `%s` property `%s` is read-only.", cls, name))
    }
    stop(sprintf("BIDS class `%s` does not have property `%s`.", cls, name))
  }
  S7::prop(x, name) <- value
  x
}

#' @export
names.bids_class <- function(x) {
  extras <- attr(x, ".bids_object_extra")
  nms <- names(extras)
  nms <- nms[!startsWith(nms, ".")]

  nms[vapply(nms, function(nm) {
    v <- extras[[nm]]
    if(!is.function(v)) { return(TRUE) }
    fmls <- names(formals(v))
    if(length(fmls) >= 1 && fmls[[1]] == "cls") {
      return(TRUE)
    }
    return(FALSE)
  }, FALSE)]
}

#' @export
`$.bids_class` <- function(x, name) {
  v <- attr(x, ".bids_object_extra")[[name]]
  if(is.function(v)) {
    fmls <- names(formals(v))
    if(length(fmls) >= 1 && fmls[[1]] == "cls") {
      return(function(...) {
        v(x, ...)
      })
    }
    return(NULL)
  }
  return(v)
}
