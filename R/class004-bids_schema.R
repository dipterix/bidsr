
# DIPSAUS DEBUG START
# schema_raw <- load_schema_file()
# do.call(bids_schema_meta, schema_raw$meta)
# schema <- bids_schema()

load_schema_file <- local({

  schema_list <- NULL
  ensure_schema_list <- function() {
    if(is.null(schema_list)) {
      schema_list <<- fastmap::fastmap()
    }
    schema_list
  }

  function(bids_version = current_bids_version()) {
    ensure_schema_list()
    if(!schema_list$has(bids_version)) {
      schema_file <- system.file("bids-schema", sprintf("schema-%s.json", bids_version), package = "bidsr")
      schema <- from_json(file = schema_file)
      schema_list$set(bids_version, schema)
    } else {
      schema <- schema_list$get(bids_version)
    }
    schema
  }

})

# ---- Schema boss -------------------------------------------------------------

bids_schema <- new_bids_class(
  name = "bids_schema",
  properties = list(
    # internal
    schema_version = bids_property_character(name = "schema_version", type = "required"),
    bids_version = bids_property_character(name = "bids_version", type = "required"),
    meta = bids_property_named_list(name = "meta"),
    # bids_property(name = "meta", class = bids_schema_meta),
    objects = bids_property_named_list(name = "objects"),
    rules = bids_property_named_list(name = "rules")
  ),
  constructor = function(bids_version = current_bids_version()) {
    schema_raw <- load_schema_file(bids_version = bids_version)
    S7::new_object(
      S7::S7_object(),
      schema_version = schema_raw$schema_version,
      bids_version = schema_raw$bids_version,
      objects = schema_raw$objects,
      rules = schema_raw$rules,
      meta = schema_raw$meta
        #do.call(bids_schema_meta, schema_raw$meta)
    )
  }
)

## `format`
S7::method(format.generic, bids_schema) <- function(x, ...) {
  sprintf("<BIDS schema> (schema: %s, BIDS specification: %s)", x@schema_version, x@bids_version)
}

get_bids_schema <- function(keys, simplify = TRUE, bids_version = current_bids_version()) {
  # schema_raw <- unlist(load_schema_file(bids_version = bids_version))
  # nms <- names(schema_raw)
  # schema_raw[startsWith(nms, sprintf("%s.", key)) | nms == key]

  schema <- load_schema_file(bids_version = bids_version)
  if(missing(keys) || !length(keys)) {
    return(schema)
  }

  re <- structure(
    names = keys,
    lapply(strsplit(keys, ".", fixed = TRUE), function(x) {
      if(anyNA(x)) { return(NULL) }
      re <- schema
      for(k in x) {
        re <- re[[k]]
      }
      re
    })
  )

  if(length(keys) == 1 && simplify) {
    re <- re[[1]]
  }

  re
}
