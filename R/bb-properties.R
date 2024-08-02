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
