

BIDSEntityFile <- new_bids_class(
  name = "BIDSEntityFile",
  abstract = TRUE,
  properties = list(
    parent_directory = bids_property_character(name = "parent_directory", type = "required", validator = validator_nonempty_string),
    data_type = bids_property_character(name = "data_type", type = "required", validator = validator_nonempty_string, final = TRUE),
    suffix = bids_property_character(name = "suffix", type = "required", validator = validator_nonempty_string, final = TRUE),
    entities = bids_property_entity_list(name = "entities"),
    # entities = bids_property(name = "entities"),
    extension = bids_property_character(name = "extension", type = "optional", setter = function(self, value) {
      value <- trimws(paste(as.character(value), collapse = ""))
      if(is.na(value) || !nzchar(value)) {
        value <- character(0L)
      }
      S7::prop(self, "extension") <- value
      self
    }),
    description = bids_property_character(name = "description", type = "optional"),
    identifier = bids_property_character(name = "identifier", type = "optional", getter = function(self) {
      # https://bids-specification.readthedocs.io/en/stable/common-principles.html#uniqueness-of-data-files
      # Data (not meta) files MUST be uniquely identified by BIDS path components (entities, data_type, suffix)
      #
      # filename registry defines the possible filename so the key is data_type + suffix
      tolower(sprintf("%s/%s", self@data_type, self@suffix))
    })
  )
)

## `get_bids_entity_rules`
S7::method(get_bids_entity_rules, BIDSEntityFile) <- function(x) {
  cls <- S7::S7_class(x)
  cls@properties$entities$entity_rules
}

## `get_bids_entity`
S7::method(get_bids_entity, BIDSEntityFile) <- function(x, key, value_only = TRUE, ifnotfound = NULL) {
  entities <- x@entities[[key]]
  if(length(entities)) {
    if(value_only) {
      re <- entities$value
      if(length(re)) {
        return(re)
      }
      return(ifnotfound)
    }
    return(entities)
  }
  return(ifnotfound)
}


## `format`
S7::method(format.generic, BIDSEntityFile) <- function(x, ...) {
  entity_str <- unlist(lapply(x@entities, format))
  re <- paste(c(entity_str, x@suffix), collapse = "_")
  extension <- x@extension
  if(length(extension)) {
    re <- sprintf("%s.%s", re, extension)
  }
  path_join(c(x@parent_directory, re))
}

## `print`
S7::method(print.generic, BIDSEntityFile) <- function(x, details = FALSE, ...) {
  fmt <- format(x)
  if(details) {
    descr <- trimws(paste(x@description, collapse = ""))
    if(nzchar(descr)) {
      descr <- sprintf("\n\t\u25b6 %s", descr)
    }
    cat(sprintf("[%s] %s%s", x@identifier, fmt, descr), ...)
  } else {
    cat(fmt)
  }
  invisible(x)
}

## `names`
S7::method(names.generic, BIDSEntityFile) <- function(x) {
  unique(c(names_bids_class_base(x), "get_bids_entity", "get_bids_entity_rules"))
}

## `[[`
S7::method(extract_bracket.generic, list(x = BIDSEntityFile, name = S7::class_any)) <- function(x, name, ...) {

  switch (
    name,
    "get_bids_entity" = {
      function(key, value_only = TRUE, ifnotfound = NULL) {
        force(key)
        get_bids_entity(x = x, key = key, value_only = value_only, ifnotfound = ifnotfound)
      }
    },
    "get_bids_entity_rules" = {
      function() {
        get_bids_entity_rules(x = x)
      }
    },
    {
      extract_bids_class_base(x, name)
    }
  )
}


test_bids_entities <- function(x, ..., .rules = list(), envir = parent.frame()) {
  rules <- c(list(...), as.list(.rules))

  # DIPSAUS DEBUG START
  # path <- "anat/sub-01_chunk-001_t1w.nii.gz"
  # x <- parse_path_bids_entity(path)
  # envir = parent.frame()
  # rules <- list(sub ~ sub %in% "01")

  for(fml in rules) {
    if(!inherits(fml, "formula")) {
      fml <- stats::as.formula(fml, env = envir)
    }
    fml_l <- as.list(fml)
    if(length(fml_l) != 3) {
      stop("Cannot resolve formula: `", deparse1(fml), "`")
    }
    entity_key <- as.character(fml_l[[2]])
    entity_val <- x@entities[[entity_key]]
    if(!is.null(entity_val) && S7::S7_inherits(entity_val, BIDSEntity)) {
      entity_val <- entity_val@value
    }

    expr <- fml_l[[3]]

    res <- tryCatch({
      suppressWarnings({
        res <- eval(expr, enclos = envir,
                    envir = structure(names = entity_key, list(entity_val)))
        res <- as.logical(res)
        isTRUE(res)
      })
    }, error = function(e) {
      FALSE
    })

    if(!res) {
      return(FALSE)
    }

  }
  return(TRUE)
}


is_child_bids_entity_file <- function(definition) {
  if(is.null(definition)) { return(FALSE) }
  if(!inherits(definition, "S7_class")) { return(FALSE) }
  if(!identical(attr(definition, "package"), "bidsr")) { return(FALSE) }
  if(identical(attr(definition, "name"), "BIDSEntityFile")) { return(TRUE) }
  tryCatch({
    is_child_bids_entity_file(definition@parent)
  }, error = function(e) { FALSE })
}

#' @title Class generator for 'BIDS' file class with entities
#' @description
#' Low-level function to generate file name definitions with entity
#' constraints; use \code{\link{parse_path_bids_entity}} instead.
#' The specification is at
#' \url{https://bids-specification.readthedocs.io/en/stable/common-principles.html#filenames}.
#'
#' @param name class name
#' @param data_type 'BIDS' file data type
#' @param suffix file suffix
#' @param schema_key schema key if explicit entity rules are required
#' @param bids_version 'BIDS' version to query the entity rules
#' @examples
#'
#'
#' # see full table at BIDS specification
#' # en/stable/appendices/entity-table.html#behavioral-data
#' #
#' # generate class definition for "Behavioral Data"
#' # Entity: Subject Session Task Acquisition Run Recording
#' # Format:
#' #   sub-<label> ses-<label> task-<label>
#' #   acq-<label> run-<index> recording-<label>
#' # suffix: events
#' # requirement: REQUIRED OPTIONAL REQUIRED OPTIONAL OPTIONAL
#' #
#'
#'
#' # ---- Basic usage ----------------------------------------
#' behavior_event_file_def <- new_bids_entity_file_class(
#'   name = "BIDSEntityFile_beh_events",
#'   data_type = "beh",
#'   suffix = "events"
#' )
#'
#' file1 <- behavior_event_file_def(
#'   parent_directory = "sub-001/beh",
#'   sub = "001", task = "test", .extension = "tsv")
#'
#' print(file1)
#'
#' file.path("root/to/path", file1)
#'
#' # How the entities are parsed?
#' file1$description
#'
#' # get entity values
#' file1$get_bids_entity("task")
#'
#' # parent directory
#' file1$parent_directory
#'
#' file1$entities$run$value
#'
#' # set entity values
#' file1$entities$run <- 2
#' file1$entities$run$index_format <- "%03d"
#'
#' file1$entities$blahblah <- "haha"
#'
#' file1
#'
#' # Relaxed entity rules generated from schema
#' # `rules.files.raw.task.events` and
#' # `rules.files.deriv.preprocessed_data.task_events_common`
#' get_bids_entity_rules(file1)
#'
#' # ---- Using BIDS schema key for specific version ------------------------
#' bids_version <- "1.10.1"
#' behavior_event_file_def <- new_bids_entity_file_class(
#'   name = "BIDSEntityFile_beh_events",
#'   data_type = "beh",
#'   suffix = "events",
#'   schema_key = "rules.files.raw.task.events",
#'   bids_version = bids_version
#' )
#'
#' file2 <- behavior_event_file_def(
#'   parent_directory = "sub-001/beh",
#'   sub = "001", task = "test", .extension = "tsv")
#'
#' file2$description
#'
#' # `desc` is no longer listed in the rules here
#' get_bids_entity_rules(file2)
#'
#'
#'
#' @export
new_bids_entity_file_class <- function(name, data_type, suffix, schema_key = NA, bids_version = current_bids_version()) {
  force(data_type)
  force(suffix)
  stopifnot(length(schema_key) == 1)

  data_type <- trimws(data_type)
  if(length(data_type) != 1 || is.na(data_type) || grepl("[\r\b\t\n /]", data_type)) {
    stop("Unable to create new BIDS entity collection for datatype: ", sQuote(data_type))
  }

  suffix <- trimws(suffix)
  if(length(suffix) != 1 || is.na(suffix) || grepl("[\r\b\t\n /]", suffix)) {
    stop("Unable to create new BIDS entity collection for suffix: ", sQuote(suffix))
  }

  if(data_type %in% c("_root", ".", "", "/") || suffix %in% c("", ".")) {
    identifier <- NULL
  } else {
    identifier <- tolower(sprintf("%s/%s", data_type, suffix))
  }

  # find required entities
  rules <- get_schema_entity_rule(identifier = identifier, schema_key = schema_key, bids_version = bids_version)

  if(!is.na(schema_key)) {
    description <- sprintf("Defined via schema_key: `%s` (BIDS version: %s)", schema_key, bids_version)
  } else if(length(identifier) == 1) {
    description <- sprintf("Defined via datatype+suffix: `%s` (BIDS version: %s)", identifier, bids_version)
  } else {
    description <- "Relaxed common rules"
  }

  entity_rules <- as.list(rules$entities)
  nms <- names(entity_rules)
  entity_args <- drop_nulls(structure(
    names = nms,
    lapply(nms, function(nm) {
      rule <- entity_rules[[nm]]
      if("required" %in% rule) { return(sprintf("%s = ", nm)) }
      if(!"prohibited" %in% rule) {
        if("index" %in% rule) {
          return(sprintf("%s = integer(0L)", nm))
        }
        return(sprintf("%s = character(0L)", nm))
      }
      return()
    })
  ))

  explicit_entity_args <- names(entity_args)
  constructor_args <- c(
    "parent_directory = ",
    unlist(entity_args),
    "... = ",
    ".list = list()",
    ".suffix = suffix",
    ".extension = character(0L)"
  )

  constructor_formals <- eval(parse(text = sprintf("alist(%s)", paste(constructor_args, collapse = ", "))))

  constructor <- function(parent_directory, ..., .list = list(), .suffix = suffix, .extension = character(0L)) {}
  environment(constructor) <- new.env(parent = asNamespace("bidsr"))
  formals(constructor) <- constructor_formals
  body(constructor) <- bquote({

    suffix <- .(suffix)
    explicit_entity_args <- .(explicit_entity_args)


    if(!identical(tolower(.suffix), tolower(suffix))) {
      stop("Cannot change suffix from ", sQuote(suffix), " to ", sQuote(.suffix), ". You can use `.suffix` to change cases only.")
    }

    entities <- structure(
      names = explicit_entity_args,
      lapply(explicit_entity_args, function(nm) {
        eval(parse(text = nm))
      })
    )
    implicit_entities <- c(list(...), .list)

    for(nm in names(implicit_entities)) {
      if(length(entities[[nm]]) == 0) {
        entities[[nm]] <- implicit_entities[[nm]]
      }
    }

    S7::new_object(
      S7::S7_object(),
      parent_directory = parent_directory,
      data_type = .(data_type),
      suffix = .suffix,
      entities = entities,
      extension = .extension,
      description = .(description)
    )

  })

  cls <- new_bids_class(
    name = name,
    parent = BIDSEntityFile,
    properties = list(
      entities = bids_property_entity_list(
        name = "entities",
        identifier = identifier,
        schema_key = schema_key,
        bids_version = bids_version
      )
    ),
    constructor = constructor
  )

  cls
}



