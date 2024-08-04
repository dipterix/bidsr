
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


#' @export
bids_map <- S7::new_class(
  name = "bids_map",
  package = "bidsr",
  parent = bids_object,
  properties = list(
    uuid = S7::new_property(
      S7::class_character,
      setter = function(self, value) {
        if(length(self@uuid)) {
          stop("bidsr::map@uuid is read-only")
        }
        self@uuid <- value
        self
      }
    ),
    impl = S7::new_property(
      S7::class_list,
      setter = function(self, value) {
        if(length(self@impl)) {
          stop("bidsr::map@impl is read-only")
        }
        self@impl <- value
        self
      }
    ),
    parent = S7::new_property(
      S7::class_any,
      setter = function(self, value) {
        if(is.null(value)) {
          self@parent <- value
          return(self)
        }
        S7::check_is_S7(value, bids_map)
        self@parent <- value
        self
      }
    ),
    search_depth = bids_required(
      class = S7::class_numeric,
      setter = function(self, value) {
        self@search_depth <- as.integer(value)
        self
      },
      validator = function(value) {
        if(!isTRUE(is.finite(value))) {
          return("BIDS map `search_depth` must be a finite integer.")
        }
        if(value > BIDS_MAP_MAX_DEPTH) {
          bids_validator_warn("BIDS map `search_depth` is too large.")
        }
        return()
      }
    ),

    format = property_format(function(self) {

        # if(is.null(self@parent)) {
        #   parent <- NULL
        # } else {
        #   parent <- self@parent@uuid
        # }
        #
        # rjson::toJSON(list(
        #   uuid = self@uuid,
        #   parent = parent,
        #   search_depth = self@search_depth,
        #   impl = as.list(self,
        #                  all.names = TRUE,
        #                  sorted = TRUE,
        #                  recursive = FALSE)
        # ), indent = json_indent())

        impl <- self@impl
        keys <- impl$keys()
        if(length(keys)) {
          keys_str <- sprintf("[%s]", paste(keys, collapse = ", "))
        } else {
          keys_str <- "  (empty)"
        }
        if(length(self@parent)) {
          parent_uuid <- sprintf("\n  (parent: %s)", self@parent@uuid)
        } else {
          parent_uuid <- ""
        }
        sprintf("<bidsr::map %s>%s\n%s",
                self@uuid,
                parent_uuid,
                keys_str)
      }
    ),

    print = property_print_format
  ),
  constructor = function(
    parent = NULL,
    search_depth = getOption("bidsr.map.seach_depth", BIDS_MAP_MAX_DEPTH)
  ) {

    S7::new_object(
      S7::S7_object(),
      parent = parent,
      impl = fastmap::fastmap(),
      search_depth = search_depth,
      uuid = uuid::UUIDgenerate()
    )
  }
)


get_bids_map_keys <- function(x, root = x, search_depth = 0L) {
  nms <- x@impl$keys()

  if(search_depth <= 0L) { return(nms) }

  parent <- x@parent
  if(is.null(parent) || identical(parent, root)) {
    return(nms)
  }

  nms2 <- Recall(parent, root, search_depth - 1L)

  sort(unique(c(nms, nms2)))
}

get_bids_map_value <- function(x, name, root, search_depth = 0L, impl = FALSE) {
  if( x@impl$has(name) ) {
    if( impl ) {
      return(x)
    }
    return( x@impl$get(name) )
  }
  if( search_depth <= 0 ) { return(NULL) }

  parent <- x@parent
  if(is.null(parent) || identical(parent, root)) {
    return( NULL )
  }
  return(Recall(
    x = parent,
    name = name,
    root = root,
    search_depth = search_depth - 1L,
    impl = impl
  ))
}

listall_bids_map <- function(
    x, root, all_names = TRUE, search_depth = 0L,
    env = new.env(parent = emptyenv())) {
  nms <- ls(env, all.names = all_names, sorted = FALSE)
  new_nms <- x@impl$keys()
  new_nms <- new_nms[!new_nms %in% nms]
  if( !all_names ) {
    new_nms <- new_nms[!startsWith(new_nms, ".")]
  }
  if(length(new_nms)) {
    list2env(x@impl$mget(new_nms), envir = env)
  }

  if( search_depth <= 0 ) { return(env) }

  parent <- x@parent
  if(is.null(parent) || identical(parent, root)) {
    return( env )
  }
  return(Recall(
    x = parent,
    root = root,
    all_names = all_names,
    search_depth = search_depth - 1L,
    env = env
  ))
}

#' @export
`names.bidsr::bids_map` <- function(x) {
  S7::check_is_S7(x, bids_map)
  get_bids_map_keys(x, x, x@search_depth)
}

#' @export
`[[.bidsr::bids_map` <- function(x, i, ..., impl = FALSE) {
  S7::check_is_S7(x, bids_map)
  get_bids_map_value(x = x, name = i, root = x, search_depth = x@search_depth, impl = impl)
}

#' @export
`[[<-.bidsr::bids_map` <- function(x, i, value) {
  x@impl$set(i, value)
  x
}

#' @export
`as.list.bidsr::bids_map` <- function(
    x, all.names = FALSE, sorted = FALSE, recursive = FALSE, ...) {

  if( recursive ) {
    search_depth <- x@search_depth
  } else {
    search_depth <- 0L
  }
  env <- new.env(parent = emptyenv())
  listall_bids_map(
    x,
    root = x,
    all_names = all.names,
    search_depth = search_depth,
    env = env
  )
  re <- as.list(env)
  if(sorted) {
    re <- re[sort(names(re))]
  }
  re
}
