

#' @export
bids_optional <- function(
    class = S7::class_any, getter = NULL, setter = NULL,
    validator = NULL, default = NULL, name = NULL) {

  v <- union_validators(
    validator_wizard("atomic", any.missing = FALSE, max.len = 1L),
    validator
  )

  S7::new_property(
    class = class,
    getter = getter,
    setter = setter,
    validator = v,
    default = default,
    name = name
  )
}

#' @export
bids_required <- function(
    class = S7::class_any, getter = NULL, setter = NULL,
    validator = NULL, default = NULL, name = NULL) {

  v <- union_validators(
    validator_wizard("atomic", any.missing = FALSE, len = 1L),
    validator
  )
  S7::new_property(
    class = class,
    getter = getter,
    setter = setter,
    validator = v,
    default = default,
    name = name
  )

}



#' @export
bids_prohibited <- function(
    class = S7::class_any, getter = NULL, setter = NULL,
    validator = NULL, default = NULL, name = NULL) {

  v <- union_validators(
    validator_wizard("atomic", any.missing = FALSE, len = 0L),
    validator
  )

  S7::new_property(
    class = class,
    getter = getter,
    setter = setter,
    validator = v,
    default = default,
    name = name
  )

}



