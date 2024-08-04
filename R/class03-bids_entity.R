


#' @export
bids_entity_value <- function(
    # type = c("optional", "required", "prohibited", "immutable"),
    initialize = NULL, validator = NULL,
    class = S7::new_union(
      S7::class_character,
      S7::class_numeric
    ), ...
) {

  bids_optional(
    class = class,
    validator = validator,
    default = initialize,
    setter = function(self, value) {
      if(!self@initialized && length(value) == 1) {
        self <- S7::set_props(self, initialized = TRUE, value = value)
      } else {
        self@value <- value
      }
      self
    },
    ...
  )

}


BIDS_ENTITY_REQUIREMENT_OPTIONS <- c("optional", "required", "prohibited")
bids_entity_requirement <- function(initialize = NULL) {
  bids_required(class = S7::class_character, default = initialize, validator = function(value) {
    if(!isTRUE(value %in% BIDS_ENTITY_REQUIREMENT_OPTIONS)) {
      return(sprintf(
        "entity requirement must be one of the followings: %s",
        paste(BIDS_ENTITY_REQUIREMENT_OPTIONS, collapse = ", ")
      ))
    }
    NULL
  })
}

BIDS_ENTITY_VALUE_TYPES <- c("label", "index", "numerical", "any")
bids_entity_type <- function(initialize = NULL) {
  bids_required(class = S7::class_character, default = initialize, validator = function(value) {
    if(!isTRUE(value %in% BIDS_ENTITY_VALUE_TYPES)) {
      return(sprintf(
        "entity type must be one of the followings: %s",
        paste(BIDS_ENTITY_VALUE_TYPES, collapse = ", ")
      ))
    }
    NULL
  })
}

bids_entity_format <- S7::new_property(
  class = S7::class_character,
  getter = function(self) {
    if(length(self@value)) {
      paste(c(self@key, self@value), collapse = "-")
    } else {
      character(0L)
    }
  }
)

#' @export
bids_entity <- S7::new_class(
  name = "bids_entity",
  package = "bidsr",
  abstract = FALSE,
  parent = bids_object,
  properties = list(

    initialized = bids_required(class = S7::class_logical, default = FALSE),
    key = property_immutable_string("key"),
    value = bids_entity_value(),
    type = bids_entity_type(),
    requirement = bids_entity_requirement(),
    format = bids_entity_format

  ),

  constructor = function(key = "",
                         requirement = BIDS_ENTITY_REQUIREMENT_OPTIONS,
                         type = BIDS_ENTITY_VALUE_TYPES) {
    requirement <- match.arg(requirement)
    type <- match.arg(type)
    key <- as.character(key)
    if(!isTRUE(nzchar(key))) {
      stop("BIDS entity key must not be empty")
    }
    S7::new_object(
      S7::S7_object(),
      key = key,
      value = S7::class_missing,
      type = type,
      requirement = requirement,
      initialized = FALSE
    )
  },

  validator = function(self) {

    key <- self@key
    if(!nzchar(key)) {
      return("BIDS entity key must not be empty")
    }
    if(!self@initialized) { return(NULL) }
    requirement <- self@requirement
    type <- self@type
    value <- self@value

    # "optional", "required", "prohibited"
    if( requirement == "required" ) {
      if(length(value) != 1) {
        return(sprintf("Please specify the value for entity `%s` (required)", self@key))
      }
    }
    if( requirement == "prohibited" ) {
      if(length(value) != 0) {
        return(sprintf("Entity `%s` must NOT be set", self@key))
      }
    } else {
      # check `type`
      # c("label", "index", "numerical", "any")
      if( type == "label" ) {
        if( !is.character(value) ) {
          return(sprintf("Entity `%s` must be a label (character)", self@key))
        }
      } else if ( type %in% c("numerical", "index") ) {
        if( !is.numeric(value) ) {
          return(sprintf("Entity `%s` must be a number", self@key))
        }
        if(type == "index" && round(value) != value) {
          return(sprintf("Entity `%s` must be an integer", self@key))
        }
      }
    }
  }
)




# e <-  bids_entity(key = "sub", type = "label", requirement = "optional")
