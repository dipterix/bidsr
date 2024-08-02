
#' @export
`[[<-.bidsr::bids_filename` <- function(x, name, value) {
  if(identical(name, "data_type")) {
    stop("Data type of a BIDS file name is a read-only property")
  }
  entity_names <- names(x@definition@entities)

  if(name %in% entity_names) {
    x@definition@entities[[name]]@value <- value
  } else {
    x@extras[[name]] <- value
  }
  x
}


#' @export
`[[.bidsr::bids_filename` <- function(x, name) {
  if(identical(name, "data_type")) {
    return( x@definition@data_type )
  }
  entities <- x@definition@entities

  if(name %in% names(entities)) {
    return(entities[[name]]@value)
  } else {
    return(x@extras[[name]])
  }
}


#' @export
`[.bidsr::bids_filename` <- function(x, i, drop = TRUE) {
  if(missing(i)) {
    i <- names(x)
    drop <- TRUE
  }
  nms <- i
  re <- structure(
    names = nms,
    lapply(nms, function(nm) { x[[nm]] })
  )
  if( drop ) {
    re <- re[ vapply(re, function(v) { length(v) == 1 }, FALSE, USE.NAMES = FALSE) ]
  }
  re
}

#' @export
`names.bidsr::bids_filename` <- function(x) {
  c(names(x@definition@entities), sort(names(x@extras)))
}

#' @export
`as.list.bidsr::bids_filename` <- function(x) {
  list(
    data_type = x@definition@data_type,
    suffix = x@definition@suffix,
    entities = x[drop = TRUE]
  )
}

#' @export
bids_filename <- S7::new_class(
  name = "bids_filename",
  package = "bidsr",
  abstract = FALSE,
  parent = bids_object,
  constructor = function(definition, ..., .list = NULL) {
    args <- c(list(...), .list)
    arg_names <- names(args)

    definition <- get_bids_filename_definition(definition)

    entities <- definition@entities
    entity_names <- names(entities)

    definition@entities <- structure(
      names = entity_names,
      lapply(entity_names, function(nm) {
        item <- entities[[nm]]
        if( nm %in% arg_names ) {
          item@value <- args[[nm]]
        } else if ( identical(item@requirement, "required") ) {
          stop(sprintf("The BIDS entity `%s` is required but missing. Please specify!", item@key))
        }
        item
      })
    )
    extras <- as.list(args[!arg_names %in% entity_names])

    S7::new_object(S7::S7_object(), definition = definition, extras = extras)
  },
  properties = list(
    definition = S7::new_property(
      class = bids_filename_definition,
      setter = function(self, value) {
        self@definition <- get_bids_filename_definition(value)
        self
      }
    ),
    extras = S7::new_property(
      class = S7::class_list,
      validator = validator_named_list,
      default = list()
    ),
    prefix = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        prefix1 <- lapply(self@definition@entities, as.character)
        extras <- self@extras
        ext_names <- names(extras)
        prefix2 <- NULL
        if(length(ext_names)) {
          ext_names <- sort(ext_names)
          prefix2 <- lapply(ext_names, function(nm) {
            v <- extras[[nm]]
            if(length(v) != 1) {
              return(NULL)
            }
            sprintf("%s-%s", nm, v)
          })

        }
        prefix <- c(unlist(prefix1), unlist(prefix2))
        prefix <- prefix[prefix != '']
        paste(c(prefix, self@definition@suffix), collapse = "_")
      }
    )
    # print = S7::new_property(
    #   class = S7::class_character,
    #   getter = function(self) {
    #     self@definition
    #   }
    # )
  )
)

