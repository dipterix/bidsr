#' @name bids_property
#' @title \code{'S7'} property for 'BIDS' classes
#' @description
#' Used in \code{property} to generate properties with constraints in
#' class generators such as \code{\link{new_bids_class}}.
#' @param name required, string, name of the property
#' @param name_meta for tabular content, the name of the meta property; default
#' is \code{"meta"}
#' @param name_content for tabular meta, the name of the content property;
#' default is \code{"content"}
#' @param lower_case_column_names for tabular content, whether to convert
#' column names to lower case; default is \code{TRUE}
#' @param preset a list of preset meta data; default is \code{NULL}
#'
#' @param class \code{'S7'} class, often pre-specified and rarely needed
#' to alter
#' @param getter,setter,validator,default see \code{\link[S7]{new_property}}
#' @param final whether the property is final once initialized; default is
#' false; this is for properties that should not be altered
#' @param type type of the property, can be \code{'required'},
#' \code{'optional'}, or \code{'prohibited'}
#' @param len for \code{type='required'}, vector length of the property;
#' default is 1
#' @param max_len for \code{type='optional'}, maximum vector length of the
#' property; default is 1
#' @param collapse for collapsed property, passed to \code{\link{paste}}
#' @param choices for properties that can only be chosen from given choices;
#' a character strings of candidate choices.
#' @param rules for 'BIDS' entities, a named list, where the keys are
#' entity keys, and values are vector of length 0 to 2, indicating the type
#' (\code{'required'}, \code{'optional'}, or \code{'prohibited'}) and data
#' format (\code{'index'}, \code{'label'}); see
#' \code{\link{bids_entity_file_registry}} for usages.
#' @param ... passed to other methods
#' @returns All functions call \code{\link[S7]{new_property}} internally.
#' @examples
#'
#'
#' cls <- new_bids_class(
#'   name = "my_class",
#'   properties = list(
#'     str = bids_property_character(
#'       name = "str",
#'       type = "required",
#'       validator = function(value) {
#'         if (length(value) == 1 &&
#'             !isTRUE(is.na(value)) && nzchar(value)) {
#'           return()
#'         }
#'         return(sprintf("Invalid `str`: %s", paste(sQuote(value), collapse = ", ")))
#'       }
#'     )
#'   ),
#'   methods = list(
#'     # read-only methods
#'     format = function(self, ...) {
#'       sprintf("my_class@str -> %s", self$str)
#'     }
#'   )
#' )
#'
#' instance <- cls(str = "aha")
#' instance
#'
#' instance$str <- "111"
#' instance
#'
#' \dontrun{
#'
#'   # what if you enter illegal values
#'
#'   cls(str = "")
#'
#'   cls(str = NA_character_)
#'
#'   cls(str = 1)
#'
#'
#' }
#'
#'
#' @export
bids_property <- function(name, class = S7::class_any, getter = NULL, setter = NULL,
                          validator = NULL, default = NULL, final = FALSE, ...) {
  if(length(name) != 1 || is.na(name) || !is.character(name) || !nzchar(name)) {
    stop("`bids_property`: property must have `name` specified.")
  }
  if(final && !is.function(setter)) {
    setter <- function(self, value) {
      if( length(S7::prop(self, name)) ) {
        stop("Property `", name, "` is read-only")
      }
      S7::prop(self, name) <- value
      self
    }
  }
  S7::new_property(
    class = class,
    getter = getter,
    setter = setter,
    validator = validator,
    default = default,
    name = name
  )
}

#' @rdname bids_property
#' @export
bids_property_optional <- function(
    name, class = S7::class_any, getter = NULL, setter = NULL,
    validator = NULL, default = NULL, max_len = 1L, ...) {

  v <- union_validators(
    validator_wizard("atomic", any.missing = TRUE, max.len = max_len),
    validator
  )

  bids_property(
    class = class,
    getter = getter,
    setter = setter,
    validator = v,
    default = default,
    name = name,
    ...
  )
}

#' @rdname bids_property
#' @export
bids_property_required <- function(
    name, class = S7::class_any, getter = NULL, setter = NULL,
    validator = NULL, default = NULL, len = 1L, ...) {

  v <- union_validators(
    validator_wizard("atomic", any.missing = FALSE, len = 1L),
    validator
  )
  bids_property(
    class = class,
    getter = getter,
    setter = setter,
    validator = v,
    default = default,
    name = name,
    ...
  )

}


#' @rdname bids_property
#' @export
bids_property_prohibited <- function(
    name, class = S7::class_any, getter = NULL, setter = NULL,
    validator = NULL, default = NULL, ...) {

  v <- union_validators(
    validator_wizard("atomic", any.missing = FALSE, len = 0L),
    validator
  )

  bids_property(
    class = class,
    getter = getter,
    setter = setter,
    validator = v,
    default = default,
    name = name,
    ...
  )

}

bids_property_internal <- function(name, type = c("optional", "required", "prohibited"), ...) {
  type <- match.arg(type)
  property <- switch(
    type,
    "required" = {
      bids_property_required(name = name, ...)
    },
    "prohibited" = {
      bids_property_prohibited(name = name, ...)
    },
    "optional" = {
      bids_property_optional(name = name, ...)
    }, {
      bids_property(name = name, ...)
    }
  )
  class(property) <- unique(c("bids_property", class(property)))
  attr(property, "requirement") <- type
  return(property)
}

#' @rdname bids_property
#' @export
bids_property_character <- function(
    name, type = c("optional", "required", "prohibited"),
    getter = NULL, setter = NULL, validator = NULL, default = NULL,
    ..., class = S7::class_character) {
  type <- match.arg(type)
  bids_property_internal(type = type, class = class, getter = getter, setter = setter,
                         validator = validator, default = default, name = name, ...)
}

#' @rdname bids_property
#' @export
bids_property_collapsed_character <- function(
    name, type = c("optional", "required", "prohibited"), collapse = " ",
    ..., class = S7::class_character) {

  type <- match.arg(type)
  force(collapse)

  bids_property_character(
    name = name,
    class = class,
    type = type,
    setter = function(self, value) {
      value <- paste(value, collapse = collapse)
      if(type != "required" && !nzchar(value)) {
        value <- character(0L)
      }
      S7::prop(self, name = name) <- value
      self
    },
    ...
  )

}

#' @rdname bids_property
#' @export
bids_property_choice <- function(name, choices, type = c("optional", "required", "prohibited"), ..., class = S7::class_character) {
  type <- match.arg(type)
  force(choices)

  bids_property_character(
    name = name,
    class = class,
    type = type,
    setter = function(self, value) {
      if(missing(value) || !length(value) || is.na(value) || !nzchar(value)) {
        value <- character(0L)
      } else {
        value <- match.arg(value, choices, several.ok = TRUE)
      }
      S7::prop(self, name = name) <- value
      self
    },
    ...
  )
}

#' @rdname bids_property
#' @export
bids_property_numeric <- function(
    name, type = c("optional", "required", "prohibited"),
    getter = NULL, setter = NULL, validator = NULL, default = NULL,
    ..., class = S7::class_numeric) {
  type <- match.arg(type)
  bids_property_internal(type = type, class = class, getter = getter, setter = setter,
                         validator = validator, default = default, name = name, ...)
}

#' @rdname bids_property
#' @export
bids_property_integerish <- function(
    name, type = c("optional", "required", "prohibited"),
    getter = NULL, setter = NULL, validator = NULL, default = NULL,
    ..., class = S7::class_numeric) {
  type <- match.arg(type)
  force(name)

  ensure_integer <- function(value) {
    suppressWarnings({
      vint <- as.integer(value)
    })
    if(is.numeric(value)) {
      if( !all(vint == value) ) {
        stop("Unable to set property `", name, "`. Values have decimal numbers that are none zero: ", value)
      }
    } else if(is.character(value)) {
      if(any((!is.na(value) & is.na(vint)))) {
        stop("Unable to set property `", name, "`. Value cannot be converted to integers: ", sQuote(value))
      }
    }
    vint
  }

  if(is.function(setter)) {
    setter_ <- function(self, value) {
      self <- setter(self, value)
      self
    }
  } else {
    setter_ <- function(self, value) {
      S7::prop(self, name) <- ensure_integer(value)
      self
    }
  }
  bids_property_internal(type = type, class = class, getter = getter, setter = setter_,
                         validator = validator, default = default, name = name, ...)
}

# bids_property_character_or_integerish <- function(
#     name, type = c("optional", "required", "prohibited"),
#     getter = NULL, setter = NULL, validator = NULL, default = NULL,
#     ..., class = S7::new_union(S7::class_numeric, S7::class_character)) {
#   type <- match.arg(type)
#   force(name)
#
#   ensure_character_or_integer <- function(value) {
#     if(is.character(value)) {
#       return(value)
#     }
#     if(is.numeric(value)) {
#
#     }
#     suppressWarnings({
#       vint <- as.integer(value)
#     })
#     if(is.numeric(value)) {
#       if( !all(vint == value) ) {
#         stop("Unable to set property `", name, "`. Values have decimal numbers that are none zero: ", value)
#       }
#     } else if(is.character(value)) {
#       if(any((!is.na(value) & is.na(vint)))) {
#         stop("Unable to set property `", name, "`. Value cannot be converted to integers: ", sQuote(value))
#       }
#     }
#     vint
#   }
#
#   if(is.function(setter)) {
#     setter_ <- function(self, value) {
#       setter(self, value)
#       self
#     }
#   } else {
#     setter_ <- function(self, value) {
#       S7::prop(self, name) <- ensure_integer(value)
#       self
#     }
#   }
#   bids_property_internal(type = type, class = class, getter = getter, setter = setter_,
#                          validator = validator, default = default, name = name, ...)
# }

#' @rdname bids_property
#' @export
bids_property_list <- function(
    name, getter = NULL, setter = NULL, validator = NULL, default = NULL,
    ..., class = S7::class_list) {
  bids_property(class = class, getter = getter, setter = setter,
                validator = validator, default = default, name = name, ...)
}

#' @rdname bids_property
#' @export
bids_property_named_list <- function(
    name, getter = NULL, setter = NULL, validator = NULL, default = list(),
    ..., class = S7::class_list) {

  v <- union_validators(
    validator_named_list,
    validator
  )
  setter_ <- function(self, value) {
    value <- as.list(value)
    if(!length(value)) {
      value <- blank_named_list()
    }
    if(is.function(setter)) {
      self <- setter(self, value)
    } else {
      S7::prop(self, name) <- value
    }
    self
  }
  bids_property(class = class, getter = getter, setter = setter_,
                validator = v, default = default, name = name, ...)
}

#' @rdname bids_property
#' @export
bids_property_unnamed_list <- function(
    name, getter = NULL, setter = NULL, validator = NULL, default = NULL,
    ..., class = S7::class_list) {

  v <- union_validators(
    validator_unnamed_list,
    validator
  )
  bids_property(class = class, getter = getter, setter = setter,
                validator = v, default = default, name = name, ...)
}

#' @rdname bids_property
#' @export
bids_property_format <- function(getter) {
  S7::new_property(
    class = S7::class_character,
    getter = getter
  )
}

#' @rdname bids_property
#' @export
bids_property_print_format <- S7::new_property(
  class = S7::class_character,
  getter = function(self) {
    format(self)
  }
)

#' @rdname bids_property
#' @export
bids_property_entity_list <- function(
    name, getter = NULL, setter = NULL, validator = NULL, default = list(),
    rules = blank_named_list(), ..., class = S7::class_list) {

  force(name)

  rules <- as.list(rules)
  default <- as.list(default)

  required_keys <- unlist(lapply(names(rules), function(nm) {
    if("required" %in% rules[[nm]]) { return(nm) }
    return()
  }))

  prohibited_rules <- unlist(lapply(names(rules), function(nm) {
    if("prohibited" %in% rules[[nm]]) { return(nm) }
    return()
  }))

  validator_ <- function(value) {
    keys <- names(value)
    missing_required <- required_keys[!required_keys %in% keys]
    if(length(missing_required)) {
      return(sprintf("Missing required entities: %s", paste(sQuote(missing_required), collapse = ", ")))
    }

    if(is.function(validator)) {
      return(validator(value))
    }
    return(NULL)
  }

  setter_ <- function(self, value) {
    value <- as.list(value)
    if(!is.list(value)) {
      stop("Property `", name, "` must be a collection (list) of entities, or a named list of entity values, where the names are entity keys.")
    }
    nms <- names(value)
    value <- lapply(seq_along(value), function(ii) {
      v <- value[[ii]]
      if(S7::S7_inherits(v, bids_entity)) {
        entity <- v
        entity_key <- entity$key
        entity_value <- entity$value
      } else {
        if(length(nms) < ii) {
          stop("Cannot convert `value` into entities": v)
        }
        entity_key <- nms[[ii]]
        entity_value <- v
        entity <- NULL
      }
      cls <- guess_entity_class(key = entity_key, object = entity, rules = rules)
      if(!S7::S7_inherits(entity, cls)) {
        entity <- cls(key = entity_key, value = entity_value)
      }
      entity
    })
    # also assign names
    names(value) <- vapply(value, "[[", FUN.VALUE = "", "key")
    if(is.function(setter)) {
      self <- setter(self, value)
    } else {
      S7::prop(self, name) <- value
    }
    return(self)
  }

  re <- bids_property(class = class, getter = getter, setter = setter_,
                      validator = validator_, default = default, name = name, ...)
  re$entity_rules <- rules
  re
}


#' @rdname bids_property
#' @export
bids_property_tabular_column_descriptor_list <- function(
    name, getter = NULL, setter = NULL, validator = NULL, default = list(),
    ..., class = S7::class_list) {

  setter_ <- function(self, value) {
    value <- as.list(value)
    if(length(value)) {
      cnames <- names(value)
      if(!length(cnames) || "" %in% cnames) {
        stop("Property `", name, "` (BIDS tabular column descripter list) must be all named.")
      }

      value <- structure(
        names = cnames,
        lapply(cnames, function(nm) {
          v <- value[[nm]]
          if(S7::S7_inherits(v, bids_tabular_column_descriptor)) { return(v) }
          return(bids_tabular_column_descriptor(.list = v))
        })
      )
    } else {
      value <- blank_named_list()
    }

    if(is.function(setter)) {
      self <- setter(self, value)
    } else {
      S7::prop(self, name) <- value
    }
    self
  }

  bids_property_named_list(
    name = name,
    getter = getter,
    setter = setter_,
    validator = validator,
    default = default,
    ...,
    class = class
  )
}


#' @rdname bids_property
#' @export
bids_property_data_frame <- function(
    name, getter = NULL, setter = NULL, validator = NULL,
    default = data.frame(), ..., class = S7::class_data.frame) {

  bids_property(
    name = name,
    class = class,
    getter = getter,
    setter = setter,
    validator = validator,
    default = default
  )
}

#' @rdname bids_property
#' @export
bids_property_tabular_content <- function(name = "content", setter = NULL, ..., name_meta = "meta", lower_case_column_names = TRUE) {
  force(name)
  force(name_meta)
  force(lower_case_column_names)

  setter_ <- function(self, value) {

    if(!data.table::is.data.table(value)) {
      value <- data.table::as.data.table(value)
    }
    if( lower_case_column_names ) {
      nms <- tolower(names(value))
      names(value) <- tolower(nms)
    } else {
      nms <- names(value)
    }

    meta <- S7::prop(self, name_meta)
    for(nm in nms) {
      descriptor <- meta$columns[[nm]]
      if(!is.list(descriptor)) {
        meta$columns[[nm]] <- list()
      }
    }

    if(is.function(setter)) {
      self <- setter(self, value)
    } else {
      S7::prop(self, name) <- value
    }

    S7::prop(self, name_meta) <- meta

    self
  }

  bids_property_data_frame(name = name, setter = setter_, ...)

}

#' @rdname bids_property
#' @export
bids_property_tabular_meta <- function(name = "meta", setter = NULL, preset = NULL, ..., name_content = "content") {
  force(name)

  preset_meta <- as_bids_tabular_meta(meta = preset)
  preset_meta_colums <- preset_meta$columns
  preset_names <- names(preset_meta_colums)

  setter_ <-  function(self, value) {
    if(!S7::S7_inherits(value, bids_tabular_meta_sidecar)) {
      value <- as_bids_tabular_meta(meta = value)
    }

    # value is already a bids_tabular_meta_sidecar now
    # check whether there are presets
    if(length(preset_meta)) {
      v_cols <- value$columns
      for(nm in preset_names) {
        if(!length(as.list(v_cols[[nm]]))) {
          v_cols[[nm]] <- preset_meta_colums[[nm]]
        }
      }
      value$columns <- v_cols
    }

    # check keys
    nms <- names(S7::prop(self, name_content))
    nms <- nms[!nms %in% names(value$columns)]
    if(length(nms)) {
      value$columns[nms] <- structure(names = nms, lapply(nms, function(o) { list() }))
    }

    if(is.function(setter)) {
      self <- setter(self, value)
    } else {
      S7::prop(self, name) <- value
    }
    self
  }
  bids_property(
    name = name, class = bids_tabular_meta_sidecar,
    setter = setter_, ...
  )
}

