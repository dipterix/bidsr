#' Low-level nested map to store key-value data with inherited structure
#' @param parent \code{NULL} if the map is at the top level, or another
#' map to inherit
#' @param search_depth integer maximum search depths; default is \code{29};
#' set options \code{'bidsr.map.search_depth'} or environment variable
#' \code{'BIDS_MAP_MAX_DEPTH'} to change the default depth
#' @returns A \code{'bids_map'} object.
#' @examples
#'
#'
#' root_map <- bids_map()
#' root_map$key1 <- 1
#' root_map$key2 <- 2
#' names(root_map)
#'
#' child_map <- bids_map(parent = root_map)
#' child_map$key3 <- 3
#' names(child_map)
#' child_map$key1
#' child_map$key2
#'
#' # mask key2
#' child_map$key2 <- "a"
#' child_map
#'
#' root_map$key2
#' child_map$key2
#'
#' # nested maps
#' grand_child <- bids_map(parent = child_map)
#'
#' # value comes from child map
#' grand_child$key2
#'
#' # remove key2 from child map
#' child_map@impl$remove("key2")
#'
#' # key2 is from root map now
#' grand_child$key2
#'
#'
#' @export
bids_map <- new_bids_class(
  name = "bids_map",
  properties = list(
    uuid = bids_property_character(name = "uuid", final = TRUE),
    impl = bids_property_list(name = "impl", final = TRUE),
    parent = bids_property(
      name = "parent",
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
    search_depth = bids_property_integerish(
      name = "search_depth",
      validator = function(value) {
        if(!isTRUE(is.finite(value))) {
          return("BIDS map `search_depth` must be a finite integer.")
        }
        if(value > BIDS_MAP_MAX_DEPTH()) {
          bids_validator_warn("BIDS map `search_depth` is too large.")
        }
        return()
      }
    ),

    format = bids_property_format(function(self) {

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
      inherit_keys <- names(self)
      inherit_keys <- inherit_keys[!inherit_keys %in% keys]
      if(length(inherit_keys)) {
        inherit_keystr <- sprintf("\n  (Inherited) [%s]", paste(inherit_keys, collapse = ", "))
      } else {
        inherit_keystr <- ""
      }
      sprintf("<bidsr::map %s>%s\n%s%s",
              self@uuid,
              parent_uuid,
              keys_str, inherit_keystr)
    })
  ),
  constructor = function(
    parent = NULL,
    search_depth = BIDS_MAP_MAX_DEPTH()
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

# `names`
S7::method(names.generic, bids_map) <- function(x) {
  S7::check_is_S7(x, bids_map)
  get_bids_map_keys(x, x, x@search_depth)
}

# `[[`
S7::method(extract_bracket.generic, bids_map) <- function(x, i, ..., impl = FALSE) {
  S7::check_is_S7(x, bids_map)
  get_bids_map_value(x = x, name = i, root = x, search_depth = x@search_depth, impl = impl)
}

# `[[<-`
S7::method(extract_set_bracket.generic, bids_map) <- function(x, i, value) {
  x@impl$set(i, value)
  x
}

# `as.list`
S7::method(as.list.generic, bids_map) <- function(
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
