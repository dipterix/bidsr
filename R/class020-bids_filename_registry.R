bids_entity_file_registry <- local({
  registry <- NULL
  ensure_registry <- function() {
    if(is.null(registry)) {
      registry <<- fastmap::fastmap(missing_default = NULL)
    }
  }
  query <- function(identifier, definition = NULL, overwrite = NA, ifnotfound = NULL) {
    ensure_registry()

    if( !missing(definition) && length(definition) == 1 ) {
      stopifnot(is_child_bids_entity_file(definition))
      if( registry$has(identifier) ) {
        if(isTRUE(overwrite)) {
          registry$set(key = identifier, value = definition)
          return(definition)
        }
        if(is.na(overwrite)) {
          return(registry$get(identifier))
        }
        stop("Registry `", identifier, "` already exists.")
      } else {
        registry$set(key = identifier, value = definition)
        return(definition)
      }
    }
    return(registry$get(identifier, missing = ifnotfound))
  }
  count <- function() {
    ensure_registry()
    registry$size()
  }
  list_all <- function(sort = TRUE) {
    ensure_registry()
    registry$as_list(sort = sort)
  }
  list(
    query = query,
    count = count,
    list_all = list_all
  )
})

bids_entity_file <- new_bids_class(
  name = "bids_entity_file",
  abstract = TRUE,
  properties = list(
    parent_directory = bids_property_character(name = "parent_directory", type = "required", validator = validator_nonempty_string),
    data_type = bids_property_character(name = "data_type", type = "required", validator = validator_nonempty_string, final = TRUE),
    suffix = bids_property_character(name = "suffix", type = "required", validator = validator_nonempty_string, final = TRUE),
    entities = bids_property_entity_list(name = "entities"),
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
S7::method(get_bids_entity_rules, bids_entity_file) <- function(x) {
  cls <- S7::S7_class(x)
  cls@properties$entities$entity_rules
}

## `get_bids_entity`
S7::method(get_bids_entity, bids_entity_file) <- function(x, key, value_only = TRUE, ifnotfound = NULL) {
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
    if(!is.null(entity_val) && S7::S7_inherits(entity_val, bids_entity)) {
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

## `format`
S7::method(format.generic, bids_entity_file) <- function(x, ...) {
  entity_str <- unlist(lapply(x@entities, format))
  re <- paste(c(entity_str, x@suffix), collapse = "_")
  extension <- x@extension
  if(length(extension)) {
    re <- sprintf("%s.%s", re, extension)
  }
  path_join(c(x@parent_directory, re))
}

## `print`
S7::method(print.generic, bids_entity_file) <- function(x, details = FALSE, ...) {
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
S7::method(names.generic, bids_entity_file) <- function(x) {
  unique(c(names_bids_class_base(x), "get_bids_entity", "get_bids_entity_rules"))
}

## `[[`
S7::method(extract_bracket.generic, list(x = bids_entity_file, name = S7::class_any)) <- function(x, name, ...) {

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

is_child_bids_entity_file <- function(definition) {
  if(is.null(definition)) { return(FALSE) }
  if(!inherits(definition, "S7_class")) { return(FALSE) }
  if(!identical(attr(definition, "package"), "bidsr")) { return(FALSE) }
  if(identical(attr(definition, "name"), "bids_entity_file")) { return(TRUE) }
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
#' For bulk generating and registering 'BIDS' file classes, see
#' \code{\link{bids_entity_file_registry}}
#'
#' @param name class name
#' @param data_type 'BIDS' file data type
#' @param suffix file suffix
#' @param entity_rules named list, where each keys is a 'BIDS' entity key (see
#' \code{\link{bids_entity}}) and the corresponding value is a character vector
#' with length of 0, 1, or 2, containing the rules of such entity; see
#' Examples' below
#' @param description file class description
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
#' behavior_event_file_def <- new_bids_entity_file_class(
#'   name = "bids_entity_file_beh_events",
#'   data_type = "beh",
#'   suffix = "events",
#'   entity_rules = list(
#'     sub = c("required", "label"),
#'     ses = c("optional", "label"),
#'     task = c("required", "label"),
#'     acq = c("optional", "label"),
#'     run = c("optional", "index"),
#'     recording = c("optional", "label")
#'   ),
#'   description = "Event timing information from a behavioral task."
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
#' file1$entities$blahblah <- "haha"
#'
#' # also set format for index entities
#' file1$entities$run$index_format <- "%03d"
#' file1
#'
#'
#' @export
new_bids_entity_file_class <- function(name, data_type, suffix, entity_rules = list(), description = character()) {
  force(data_type)
  force(suffix)
  force(description)

  data_type <- trimws(data_type)
  if(length(data_type) != 1 || is.na(data_type) || grepl("[\r\b\t\n ]", data_type)) {
    # trimws(data_type) %in% c("", ".") ||
    stop("Unable to create new BIDS entity collection for datatype: ", sQuote(data_type))
  }

  # find required entities
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

  constructor <- function(parent_directory, ..., .list = list(), .suffix = suffix, .extension = character(0L)) {
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
      data_type = data_type,
      suffix = suffix,
      entities = entities,
      extension = .extension,
      description = description
    )
  }
  formals(constructor) <- constructor_formals

  entity_rules <- as.list(entity_rules)

  cls <- new_bids_class(
    name = name,
    parent = bids_entity_file,
    properties = list(
      entities = bids_property_entity_list(
        name = "entities",
        rules = entity_rules
      )
    ),
    constructor = constructor
  )

  cls
}



#' @title Parse 'BIDS' entities from file path
#' @param path path to the entity file, recommended to input the absolute path
#' or relative path from the 'BIDS' root directory
#' @param auto_new whether to automatically generate class definition if
#' missing; default is true
#' @returns A \code{'bids_entity_file'} instance.
#' @examples
#'
#'
#' path <- "anat/sub-01_chunk-001_t1w.nii.gz"
#'
#' # --- parse ------------------------------------------------
#' parsed_filename <- parse_path_bids_entity(path)
#' parsed_filename
#'
#' parsed_filename$get_bids_entity("sub")
#'
#' # alternatively
#' parsed_filename$entities$sub$value
#'
#' # data type is `anat` imaging
#' parsed_filename$data_type
#'
#' # data is T1-weighted
#' parsed_filename$suffix
#'
#' # --- usage ------------------------------------------------
#' # use it as character
#' file.path("/path/to/bids/dir/sub-01", parsed_filename)
#'
#' # modify
#' parsed_filename$entities$task <- "special"
#'
#' # new file path: anat/sub-01_task-special_chunk-001_T1w.nii.gz
#' parsed_filename
#'
#' # ---- schema -----------------------------------------------
#' # get BIDS entity rules
#' parsed_filename$get_bids_entity("task")
#'
#' # get entity rules
#' parsed_filename$get_bids_entity_rules()
#'
#'
#' @export
parse_path_bids_entity <- function(path, auto_new = TRUE) {
  stopifnot(length(path) == 1 && !is.na(path))

  data_type <- basename(dirname(path))
  if(
    startsWith(data_type, "ses-") ||
    startsWith(data_type, "sub-")
  ) {
    # This is a root file for session/subject
    # e.g. sub-06/ses-ieeg01/sub-06_ses-ieeg01_scans.tsv
    data_type <- "root"
    auto_new <- TRUE
  }
  file_name <- basename(path)
  parsed <- strsplit(file_name, "_")[[1]]
  # check if connector exists
  parsed_rev <- rev(parsed)
  has_dash_rev <- vapply(parsed_rev, function(xp) {
    grepl("-", xp, fixed = TRUE)
  }, FALSE)

  file_ending <- NULL
  for(ii in seq_along(parsed_rev)) {
    if( ii == 1 || !has_dash_rev[[ii]] ) {
      file_ending <- c(parsed_rev[[ii]], file_ending)
    } else if(has_dash_rev[[ii]]) {
      break
    }
  }
  parsed <- c(
    parsed[seq_len( length(parsed) - length(file_ending) )],
    paste(file_ending, collapse = "_")
  )

  n_parsed <- length(parsed)
  postfix <- parsed[[n_parsed]]

  postfix <- strsplit(postfix, ".", fixed = TRUE)[[1]]
  suffix <- postfix[[1]]
  ext <- paste(postfix[-1], collapse = ".")
  identifier <- tolower(sprintf("%s/%s", data_type, suffix))

  definition <- bids_entity_file_registry_get(identifier, ifnotfound = NULL)

  if(is.null(definition)) {
    if(!auto_new) {
      stop("No entity definition for file with data-type `", data_type, "` with suffix `", suffix, "`.")
    }
    cls_name <- sprintf("bids_entity_file_%s_%s", data_type, tolower(suffix))
    definition <- new_bids_entity_file_class(
      name = cls_name,
      data_type = data_type,
      suffix = suffix,
      description = sprintf("Temporary class of BIDS file entity, generated from `%s`", path)
    )
  }

  entities <- parsed[-n_parsed]
  entities <- do.call("rbind", lapply(strsplit(entities, "-", fixed = TRUE), function(x) {
    c(x[[1]], paste(x[-1], collapse = "-"))
  }))

  args <- structure(
    names = entities[,1],
    as.list(entities[, 2])
  )
  args$.extension <- ext
  args$.suffix <- suffix
  args$parent_directory <- dirname(path)
  do.call(definition, args)

}


#' @name bids_entity_file_registry
#' @title Generate 'BIDS' entity file-name class
#' @description
#' Rule-based 'BIDS' entity file names, see class generator at
#' \code{\link{new_bids_entity_file_class}}; for entity tables; see
#' \url{https://bids-specification.readthedocs.io/en/stable/appendices/entity-table.html}
#' @param definition_table data table or a path to a 'CSV' file where 'BIDS'
#' entities are defined for each combination of data type and suffix. The
#' columns must start with \code{'DataType'} and \code{'Suffix'}, followed by
#' data entities; See 'Examples'.
#' @param entity_table table of two columns: \code{'Name'} (entity keys) and
#' \code{'Type'}) entity data type; See 'Examples'.
#' @param data_type_table,suffix_table used to generate descriptions; optional
#' @param overwrite whether to overwrite existing registry if two definitions
#' with the same identifier coincide; default is \code{NA}, assuming two
#' definitions are identical, and will not overwrite existing ones; set to
#' \code{TRUE} to always force registering the new definitions; set to
#' \code{FALSE} and the script will error out if existing definitions have been
#' already registered.
#' @param identifier file registry identifier, automatically derived from
#' data type and suffix; format is \code{'datatype/suffix'}
#' @param ifnotfound default to return is definition is not found
#' @param data_types constraint returns to be under certain data types; default
#' is \code{NA} (return the entire registry)
#' @returns For \code{parse_bids_file_entity_registry}, returns a list of
#' entity identifiers and definitions; \code{register_bids_file_entiries}
#' registers these definitions to a registry, by identifiers, and returns the
#' number of total definitions registered.
#'
#' @examples
#'
#' # ---- Example tables --------------------------------------------
#'
#' definition_table <- system.file(
#'   "definitions", "MagneticResonanceImaging.csv",
#'   package = "bidsr")
#'
#' print(head(read.csv(definition_table)))
#'
#'
#' entity_table <- system.file(
#'   "definitions", "MagneticResonanceImagingEntity.csv",
#'   package = "bidsr")
#'
#' print(head(read.csv(entity_table)))
#'
#'
#' data_type_table <- system.file(
#'   "definitions", "glossary", "DataType.csv", package = "bidsr")
#'
#' print(head(read.csv(data_type_table)))
#'
#'
#' suffix_table <- system.file(
#'   "definitions", "glossary", "Suffix.csv", package = "bidsr")
#'
#' print(head(read.csv(suffix_table)))
#'
#' # ---- Parse ------------------------------------------------
#' parsed <- parse_bids_file_entity_registry(
#'   definition_table = definition_table,
#'   entity_table = entity_table,
#'   data_type_table = data_type_table,
#'   suffix_table = suffix_table
#' )
#'
#' parsed[[1]]
#'
#' # ---- Register to registry for querying --------------------
#' register_bids_file_entiries(
#'   definition_table = definition_table,
#'   entity_table = entity_table,
#'   data_type_table = data_type_table,
#'   suffix_table = suffix_table
#' )
#'
#' # example using definition
#' t1_definition <- bids_entity_file_registry_get("anat/t1w")
#'
#' filename <- t1_definition(
#'   parent_directory = "anat",
#'   sub = "001",
#'   ses = "001",
#'   task = "001",
#'   acq = "ecog",
#'   .extension = "nii.gz"
#' )
#'
#' # use formatting
#' format(filename)
#'
#' file.path("BIDS/directory/to", filename)
#'
#' # with descriptions
#' print(filename, details = TRUE)
#'
#' # get subject value
#' filename$entities$sub$value
#'
#'
#' @export
parse_bids_file_entity_registry <- function(
    definition_table, entity_table,
    # glossary
    data_type_table = NULL, suffix_table = NULL) {

  if(is.null(data_type_table)) {
    data_type_table <- system.file("definitions", "glossary", "DataType.csv", package = "bidsr")
  }
  if(is.null(suffix_table)) {
    suffix_table <- system.file("definitions", "glossary", "Suffix.csv", package = "bidsr")
  }

  # DIPSAUS DEBUG START
  # definition_table <- system.file("definitions", "MagneticResonanceImaging.csv", package = "bidsr")
  # entity_table <- system.file("definitions", "MagneticResonanceImagingEntity.csv", package = "bidsr")
  # data_type_table <- system.file("definitions", "glossary", "DataType.csv", package = "bidsr")
  # suffix_table <- system.file("definitions", "glossary", "Suffix.csv", package = "bidsr")
  # ii=1

  load_csv_if_is_path <- function(x) {
    if(is.data.frame(x)) { return(x) }
    if(!file_exists(x)) {
      stop("Path `", x, "` does not exists")
    }
    suppressWarnings({ utils::read.csv(x) })
  }
  definition_table <- load_csv_if_is_path(definition_table)
  entity_table <- load_csv_if_is_path(entity_table)
  data_type_table <- load_csv_if_is_path(data_type_table)
  suffix_table <- load_csv_if_is_path(suffix_table)

  # Clean data_type_table and suffix_table for later use
  data_type_table$Name <- tolower(data_type_table$Name)
  suffix_table$Name <- tolower(suffix_table$Name)

  # parse entity_table:
  # make sure entity_table `Name` is unique
  entity_table$Name <- tolower(entity_table$Name)
  duplicated_entity_names <- entity_table$Name[duplicated(entity_table$Name)]
  if(length(duplicated_entity_names)) {
    stop("Entity table has duplicated entities: ", paste(sQuote(duplicated_entity_names), collapse = ", "))
  }
  # rules
  entity_rules <- structure(
    names = entity_table$Name,
    as.list(tolower(entity_table$Type))
  )

  # parse `definition_table`
  defs <- lapply(seq_len(nrow(definition_table)), function(ii) {
    row <- as.list(definition_table[ii, ])
    datatype <- tolower(row$DataType)
    suffix <- row$Suffix
    rules <- row[-c(1, 2)]
    rules <- structure(
      names = tolower(names(rules)),
      as.list(tolower(as.vector(rules)))
    )
    rule_names <- unique(c(names(rules), names(entity_rules)))
    rules <- structure(
      names = rule_names,
      lapply(rule_names, function(key) {
        c(rules[[key]], entity_rules[[key]])
      })
    )

    cls_name <- sprintf("bids_entity_file_%s_%s", datatype, tolower(suffix))

    # get description
    descr <- trimws(c(
      suffix_table$Description[ suffix_table$Name == tolower(suffix) ],
      data_type_table$FullName[ data_type_table$Name == datatype ]
    ))
    descr <- descr[descr != ""]
    if(length(descr)) {
      descr <- paste(descr, collapse = ", ")
    }

    cls <- new_bids_entity_file_class(cls_name, data_type = datatype, entity_rules = rules, suffix = suffix, description = descr)
    list(
      identifier = sprintf("%s/%s", datatype, tolower(suffix)),
      definition = cls
    )
  })
  defs
}

#' @rdname bids_entity_file_registry
#' @export
register_bids_file_entiries <- function(
    definition_table, entity_table,
    # glossary
    data_type_table = NULL, suffix_table = NULL, overwrite = NA) {

  results <- parse_bids_file_entity_registry(
    definition_table = definition_table,
    entity_table = entity_table,
    data_type_table = data_type_table,
    suffix_table = suffix_table
  )
  if(length(results)) {
    lapply(results, function(item) {
      bids_entity_file_registry$query(identifier = item$identifier, definition = item$definition, overwrite = overwrite)
      return()
    })
  }
  invisible(bids_entity_file_registry$count())
}

#' @rdname bids_entity_file_registry
#' @export
bids_entity_file_registry_get <- function(identifier, ifnotfound = NULL, overwrite = NA) {
  if(!isFALSE(overwrite) && length(ifnotfound) == 1 && inherits(ifnotfound, "S7_class")) {
    definition <- ifnotfound
  } else {
    definition <- NULL
  }
  bids_entity_file_registry$query(identifier = identifier, definition = definition, overwrite = overwrite, ifnotfound = ifnotfound)
}

#' @rdname bids_entity_file_registry
#' @export
bids_entity_file_registry_list <- function(data_types = NA) {
  re <- bids_entity_file_registry$list_all()
  if(length(data_types) != 1 || !is.na(data_types)) {
    dtypes <- vapply(strsplit(names(re), split = "/", fixed = TRUE), function(x){x[[1]]}, "")
    re <- re[dtypes %in% data_types]
  }
  re
}

#' #' @export
#' register_bids_filename_definition <- function(definition, overwrite = FALSE) {
#'   S7::check_is_S7(definition, bids_filename_definition)
#'   def_key <- tolower(as.character(definition))
#'   if(bids_entity_table$has(def_key)) {
#'     if( is.na( overwrite ) ) {
#'       warning(sprintf(
#'         "Overwriting existing BIDS filename definition `%s`. If you want to supress this warning, set `overwrite=TRUE`",
#'         def_key))
#'       overwrite <- TRUE
#'     }
#'     if( !overwrite ) {
#'       stop(sprintf(
#'         "BIDS filename definition `%s` already exists. If you want to overwrite this definition, set `overwrite=TRUE`",
#'         def_key))
#'     }
#'   }
#'   bids_entity_table$set(key = def_key, value = definition)
#' }
#'
#' #' @export
#' get_bids_filename_definition <- function(definition) {
#'   if(is.character(definition)) {
#'     def <- bids_entity_table$get(tolower(definition), missing = NULL)
#'     if(is.null(def)) {
#'       stop("BIDS filename definition does not exists for: ",
#'            definition,
#'            ". If this definition does not exist, you can create your own. ",
#'            "See `?register_bids_filename_definition` for details")
#'     }
#'     definition <- def
#'   }
#'   S7::check_is_S7(definition, bids_filename_definition)
#'   definition
#' }
