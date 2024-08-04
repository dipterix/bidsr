make_collapsed_string_property <- function(
    name, validator = NULL,
    requirement = c("optional", "required"),
    ...) {

  requirement <- match.arg(requirement)

  switch(
    requirement,
    "optional" = {
      bids_optional(
        class = S7::class_character,
        setter = function(self, value) {
          value <- paste(value, collapse = "")
          if(!nzchar(value)) {
            value <- character(0L)
          }
          S7::prop(self, name = name) <- value
          self
        },
        validator = validator,
        name = name,
        ...
      )
    },
    {
      bids_required(
        class = S7::class_character,
        setter = function(self, value) {
          value <- paste(value, collapse = "")
          S7::`prop<-`(self, name = name, value = value)
          self
        },
        validator = validator,
        name = name,
        ...
      )
    }
  )

}


property_named_list <- S7::new_property(
  class = S7::class_list,
  validator = validator_named_list
)


property_nonempty_string <- S7::new_property(
  class = S7::class_character,
  validator = validator_nonempty_string
)

property_format <- function(fun) {
  S7::new_property(
    class = S7::class_character,
    getter = fun
  )
}

property_print_format <- S7::new_property(
  class = S7::class_character,
  getter = function(self) {
    format(self)
  }
)


property_immutable_string <- function(name, initialize = NULL, validator = NULL) {
  force(name)
  if(!length(initialize)) {
    initialize <- ""
  }
  ensure_and_get_key <- function(self) {
    key <- get_prop_in_getter(self, name)
    if(length(key) != 1) {
      key <- as.character(initialize)
      attr(self, name) <- key
    }
    key
  }
  bids_required(
    class = S7::class_character,
    default = initialize,
    validator = validator,
    getter = ensure_and_get_key,
    setter = function(self, value) {
      key <- ensure_and_get_key(self)
      if (nzchar(key)) {
        if(!identical(key, value)) {
          stop(sprintf("Property `%s` is immutable", name))
        }
      } else {
        S7::prop(self, name) <- value
      }
      self
    }
  )
}
