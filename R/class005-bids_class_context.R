# DIPSAUS DEBUG START
# schema_raw <- load_schema_file()
# schema <- bids_schema()
# meta_schema <- schema$meta$context


bids_context <- new_bids_class(
  name = "bids_context",
  abstract = FALSE,
  properties = list(
    meta_schema = bids_property(name = "meta_schema", class = S7::class_list),
    store = bids_property(name = "store", class = S7::class_environment)
  ),
  constructor = function(meta_schema, value = NULL, children = list()) {
    store <- new.env(parent = emptyenv())
    store$._value_. <- value

    property_names <- names(meta_schema$properties)
    # required_property_names <- meta_schema$required

    children_names <- sapply(children, "[[", "context_name")
    children <- as.list(children)
    names(children) <- children_names

    lapply(property_names, function(pname) {
      child <- children[[pname]]
      if(S7::S7_inherits(child, bids_context)) {
        store[[pname]] <- child
        return()
      }
      store[[pname]] <- bids_context(meta_schema = meta_schema$properties[[pname]])
    })

    S7::new_object(
      S7::S7_object(),
      meta_schema = meta_schema,
      store = store
    )

  },
  methods = list(
    set_value = function(self, value) {
      property_names <- names(self@meta_schema@properties)
      if(length(property_names)) {
        vnames <- names(value)
        property_names <- property_names[property_names %in% vnames]
        lapply(property_names, function(nm) {
          self@store[[nm]]$set_value(value[[nm]])
        })
        vnames <- vnames[!vnames %in% property_names]
        if(length(vnames)) {
          value <- value[vnames]
        } else {
          value <- NULL
        }
      }
      switch(
        self@meta_schema@type,
        "string" = {
          self@store$._value_. <- as.character(value)
        },
        "object" = {
          self@store$._value_. <- value
        },
        {
        stop("Unsupported context type: ", sQuote(self@meta_schema@type), " (", self@meta_schema@context_name, ").")
        }
      )
      invisible(self)
    },
    get_value = function(self, validate = TRUE) {
      re <- self@store$._value_.
      switch(
        self@meta_schema@type,
        "string" = { re <- as.character(re) },
        "object" = {
          if(is.null(re)) {
            re <- list()
          }
        },
        "array" = {
          re <- as.vector(re)
        },
        "integer" = {
          re <- as.integer(re)
        },
        "number" = {
          re <- as.numeric(re)
        },
        {
          stop("Unsupported context type: ", sQuote(self@meta_schema@type), "\nSchema:\n", format(self@meta_schema))
        }
      )
      property_names <- names(self@meta_schema@properties)
      if(length(property_names)) {
        required_names <- self@meta_schema@required
        props <- structure(
          names = property_names,
          lapply(property_names, function(nm) {
            v <- self@store[[nm]]$get_value(validate = validate)
            if(nm %in% required_names && !length(v)) {
              if(validate) {
                stop("Invalid context, property `", self@meta_schema@context_name, ".", nm, "` is required but empty.")
              }
            }
            v
          })
        )
        re <- c(
          re,
          props
        )
      }
      re
    }
  )
)

