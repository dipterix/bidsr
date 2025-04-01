#' @name BIDSEntity
#' @author Zhengjia Wang
#' @title Class definitions of 'BIDS' entity
#' @description
#' A 'BIDS' entity is an attribute that can be associated with a file,
#' contributing to the identification of that file as a component of its
#' file-name in the form of a hyphen-separated key-value pair. The specification
#' can be found at \url{https://bids-specification.readthedocs.io/en/stable/common-principles.html#entities}.
#' @param key (string, required) A short string, typically a compression of the
#' entity name, which uniquely identifies the entity when part of a file-name.
#' @param value A string (label) or a non-negative integer (index); the
#' requisite form of the value that gets specified alongside the key whenever
#' the entity appears in a file-name. For each entity, the value is of one of
#' two possible types:
#' @param index_format for index entities, how to format index values (e.g.
#' padding zeros) when formatted as string; default is without padding
#' \describe{
#' \item{Index:}{A non-negative integer, potentially zero-padded for
#' consistent width.}
#' \item{Label:}{An alphanumeric string. Note that labels must not collide
#' when casing is ignored (\code{bidsr} does not validate this).}
#' }
#' @returns A 'BIDS' entity object.
#' @examples
#'
#'
#' entity_int <- BIDSEntity_index_optional(key = "run", value = "001")
#' entity_int$value <- integer()
#'
#' print(entity_int) # nothing will be printed out
#'
#' # subject entity
#' entity_subject <- BIDSEntity_any_required(key = "sub", value = "HUP225")
#'
#' print(entity_subject)
#'
#' # index
#' entity_subject$value <- 1
#'
#' print(entity_subject)
#'
#' # format index
#' entity_subject$index_format <- "%03d"
#' print(entity_subject)
#'
#'
#' # trying to set invalid values will result in errors
#' try({
#'   BIDSEntity_index_required(key = "run")
#' })
#'
#'
#' entity_int <- BIDSEntity_index_required(key = "run", value = "001")
#'
#' # trying to unset require entity
#' try({
#'   entity_int$value <- integer()
#' })
#'
#' # trying to set invalid entity
#' try({
#'   entity_int$value <- "asdad"
#' })
#'
#' # trying to set prohibited entiry
#' try({
#'   BIDSEntity_index_prohibited("invalid", 123)
#' })
#'
#'
NULL

BIDSEntity <- new_bids_class(
  name = "BIDSEntity",
  abstract = TRUE,
  properties = list(

    # initialized = bids_required(class = S7::class_logical, default = FALSE),
    key = bids_property_character(name = "key", type = "required", final = TRUE, validator = validator_nonempty_string),
    value = bids_property(name = "value"),
    index_format = bids_property_character(name = "index_format", type = "required", default = "%d")

  ),
  validator = function(self) {
    if(anyNA(self$value)) {
      return("BIDS entity value cannot be NA. Use `character(0)` (label) or `integer(0)` (index) to mark as missing")
    }
    return()
  }
)

## `format`
S7::method(format.generic, BIDSEntity) <- function(x, ...) {
  v <- x@value
  if(length(v) && !is.na(v)) {
    if(is.numeric(v)) {
      v <- sprintf(x@index_format, v)
    }
    paste(c(x@key, v), collapse = "-")
  } else {
    character(0L)
  }
}

new_bids_entity_class <- function(value, type = BIDS_ENTITY_REQUIREMENT_OPTIONS, ..., value_class_name = class(value)) {

  if(value_class_name %in% c("S7_union", "S7_property")) {
    stop("`new_bids_entity_class`: Please specify `value_class_name`.")
  }
  if(inherits(value, "bids_property")) {
    type <- match.arg(attr(value, "requirement"), BIDS_ENTITY_REQUIREMENT_OPTIONS)
    value_prop <- value
  } else {
    type <- match.arg(type)
    value_prop <- bids_property_internal(name = "value", type = type, class = value, ...)
  }

  cls_name <- sprintf("BIDSEntity_%s_%s", value_class_name, type)

  cls <- new_bids_class(
    name = cls_name,
    parent = BIDSEntity,
    properties = list(

      # key = bids_property_character(name = "key", type = "required", final = TRUE, validator = validator_nonempty_string),
      value = value_prop
      # index_format = bids_property_character(name = "index_format", type = "required", default = "%d")

    ),
    # validator = function(self) {
    #   if(anyNA(self$value)) {
    #     return("BIDS entity value cannot be NA. Use `character(0)` (label) or `integer(0)` (index) to mark as missing")
    #   }
    #   return()
    # }
  )

  S7::method(format.generic, cls) <- function(x, ...) {
    v <- x@value
    if(type != "prohibited" && length(v) && !is.na(v)) {
      if(is.numeric(v)) {
        v <- sprintf(x@index_format, v)
      }
      paste(c(x@key, v), collapse = "-")
    } else {
      character(0L)
    }
  }

  cls
}


#' @rdname BIDSEntity
#' @export
BIDSEntity_label_required <- new_bids_entity_class(
  value = S7::class_character,
  type = "required",
  value_class_name = "label"
)

#' @rdname BIDSEntity
#' @export
BIDSEntity_label_optional <- new_bids_entity_class(
  value = S7::class_character,
  type = "optional",
  value_class_name = "label"
)

#' @rdname BIDSEntity
#' @export
BIDSEntity_label_prohibited <- new_bids_entity_class(
  value = S7::class_character,
  type = "prohibited",
  value_class_name = "label"
)

get_index_format <- function(value) {
  value <- paste(value, collapse = "")
  if(!grepl(pattern = ".", value, fixed = TRUE) && startsWith(value, "0")) {
    fmt <- sprintf("%%0%dd", nchar(value))
  } else {
    fmt <- "%d"
  }
  fmt
}

ensure_entity_index <- function(value) {
  suppressWarnings({
    vint <- as.integer(value)
  })
  if(is.numeric(value)) {
    if( !all(vint == value) ) {
      stop("Unable to set BIDS entity as index value. Values have decimal numbers that are none zero: ", value)
    }
  } else if(is.character(value)) {
    if(any((!is.na(value) & is.na(vint)))) {
      stop("Unable to set BIDS entity as index value. Value cannot be converted to integers: ", sQuote(value))
    }
  }
  vint
}

#' @rdname BIDSEntity
#' @export
BIDSEntity_index_required <- new_bids_entity_class(
  value = bids_property_integerish(
    name = "value",
    type = "required",
    validator = validator_nonnegative_intergerish,
    setter = function(self, value) {
      self@index_format <- get_index_format(value)
      self@value <- ensure_entity_index(value)
      self
    }
  ),
  value_class_name = "index"
)

#' @rdname BIDSEntity
#' @export
BIDSEntity_index_optional <- new_bids_entity_class(
  value = bids_property_integerish(
    name = "value",
    type = "optional",
    validator = validator_nonnegative_intergerish,
    setter = function(self, value) {
      self@index_format <- get_index_format(value)
      self@value <- ensure_entity_index(value)
      self
    }
  ),
  value_class_name = "index"
)

#' @rdname BIDSEntity
#' @export
BIDSEntity_index_prohibited <- new_bids_entity_class(
  value = bids_property_integerish(
    name = "value",
    type = "prohibited",
    validator = validator_nonnegative_intergerish,
    setter = function(self, value) {
      self@index_format <- get_index_format(value)
      self@value <- ensure_entity_index(value)
      self
    }
  ),
  value_class_name = "index"
)

#' @rdname BIDSEntity
#' @export
BIDSEntity_any_required <- new_bids_entity_class(
  value = S7::new_union(S7::class_character, S7::class_numeric),
  type = "required",
  value_class_name = "any",
  validator = function(value) {
    if(is.character(value)) {
      return(validator_nonempty_string(value))
    } else {
      return(validator_nonnegative_intergerish(value))
    }
  }
)

#' @rdname BIDSEntity
#' @export
BIDSEntity_any_optional <- new_bids_entity_class(
  value = S7::new_union(S7::class_character, S7::class_numeric),
  type = "optional",
  value_class_name = "any",
  validator = function(value) {
    if(length(value)) {
      if (is.character(value)) {
        return(validator_nonempty_string(value))
      } else {
        return(validator_nonnegative_intergerish(value))
      }
    }
    return(NULL)
  }
)

#' @rdname BIDSEntity
#' @export
BIDSEntity_any_prohibited <- new_bids_entity_class(
  value = S7::new_union(S7::class_character, S7::class_numeric),
  type = "prohibited",
  value_class_name = "any",
  validator = function(value) {
    if(length(value) > 0) {
      return("Entity is prohibited, invalid value: ", paste(value, collapse = ", "))
    }
    return()
  }
)



# e <-  BIDSEntity(key = "sub", type = "label", requirement = "optional")

# ---- utilities ------------------------------------------------

get_entity_rule <- function(object) {
  cls_name <- class(object)
  cls_name <- cls_name[startsWith(cls_name, "bidsr::BIDSEntity_")]
  if(!length(cls_name)) {
    return(c("any", "optional"))
  }
  entity_rule <- gsub("^bidsr::BIDSEntity_", "", cls_name[[1]])
  entity_rule <- tolower(strsplit(entity_rule, "_")[[1]])

  if(entity_rule[[1]] %in% BIDS_ENTITY_VALUE_TYPES) {
    entity_rule[[1]] <- "any"
  }
  if(entity_rule[[2]] %in% BIDS_ENTITY_REQUIREMENT_OPTIONS) {
    entity_rule[[2]] <- "optional"
  }
  return(entity_rule)
}


guess_entity_class <- function(key, object = NULL, rules = list()) {
  rule0 <- get_entity_rule(object)

  rule <- rules[[key]]

  value_t <- BIDS_ENTITY_VALUE_TYPES[BIDS_ENTITY_VALUE_TYPES %in% rule]
  if(length(value_t)) {
    rule0[[1]] <- value_t[[1]]
  }

  req_t <- BIDS_ENTITY_REQUIREMENT_OPTIONS[BIDS_ENTITY_REQUIREMENT_OPTIONS %in% rule]
  if(length(req_t)) {
    rule0[[2]] <- req_t[[1]]
  }
  cls_name <- sprintf("BIDSEntity_%s_%s", rule0[[1]], rule0[[2]])
  cls <- get(cls_name, envir = asNamespace("bidsr"))
  return(cls)
}

