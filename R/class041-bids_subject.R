#' @name bids_subject
#' @title 'BIDS' subject class
#' @param project 'BIDS' project instance, see \code{\link{bids_project}}, or
#' a path to the 'BIDS' project
#' @param ... passed to the constructor of \code{\link{bids_project}}, when
#' \code{project} is a character string
#' @param subject_code character, subject code with or without the leading
#' \code{'sub-'}. The subject code, after trimming the leading entity key,
#' should not contain any additional dash (\code{'-'})
#' @param strict whether to check if the subject folders exist, can be
#' logical or characters; when \code{strict} is character strings, choices can
#' be \code{'raw'} (checking raw-data directory) and/or \code{'source'} (
#' for source-data directory); \code{strict=TRUE} is equivalent to checking
#' both; default is \code{'raw'}. There is no checks on derivatives.
#' @returns A 'BIDS' subject instance.
#'
#' @examples
#'
#' # Run `download_bids_examples()` first
#' examples <- download_bids_examples(test = TRUE)
#' if(!isFALSE(examples)) {
#'
#'   project_path <- file.path(examples, "ieeg_epilepsy_ecog")
#'
#'   project <- bids_project(
#'     path = project_path,
#'     raw_data_relpath = ".",
#'     derivative_data_relpath = "derivatives"
#'   )
#'
#'
#'   subject <- bids_subject(project = project, subject_code = "ecog01",
#'                           strict = FALSE)
#'
#'   storage_root <- resolve_bids_path(subject, storage = "raw")
#'
#'   query_bids(subject, "ieeg")
#'
#' }
#'
#' @export
bids_subject <- new_bids_class(
  name = "bids_subject",
  properties = list(
    project = bids_property(
      name = "project", class = bids_project
    ),
    subject_code = bids_property_character(
      name = "subject_code",
      type = "required",
      setter = function(self, value) {
        if(length(value) != 1) {
          stop("Subject code length must be 1.")
        }
        value <- as.character(value)
        if(startsWith(tolower(value), "sub-")) {
          value <- gsub("^sub-", "", value, ignore.case = TRUE)
        }
        if(grepl("-", value)) {
          stop("BIDS subject code cannot contain dash `-`.")
        }
        self@subject_code <- value
        self
      }
    )
  ),
  constructor = function(project, subject_code, ..., strict = "raw") {
    subject_code <- gsub("^sub-", "", as.character(subject_code), ignore.case = TRUE)

    if(!S7::S7_inherits(project, bids_project)) {
      project <- bids_project(path = project, ...)
    }
    if(isTRUE(strict)) {
      strict <- c("raw", "source")
    } else if (is.character(strict)) {
      strict <- strict[strict %in% c("raw", "source")]
    } else {
      strict <- NULL
    }
    if(length(strict)) {
      for(storage in strict) {
        relpath <- S7::prop(project, sprintf("%s_data_relpath", storage))
        if(!nzchar(relpath) || trimws(relpath) %in% c(".", "")) {
          relpath <- NULL
        }
        storage_root <- path_join(c(project@path, relpath, sprintf("sub-%s", subject_code)))
        if(!dir_exists(storage_root)) {
          stop("Unable to find valid directory: ", storage_root)
        }
      }
    }
    S7::new_object(
      S7::S7_object(),
      project = project,
      subject_code = subject_code
    )
  }
)



## `print` generic
S7::method(print.generic, bids_subject) <- function(x, ...) {
  cat(sprintf("<BIDS Subject> `sub-%s` (project `%s`)\n", x@subject_code, x@project@name))
  invisible(x)
}

## `resolve_bids_path` generic
S7::method(resolve_bids_path, bids_subject) <- function(x, ..., storage = c("raw", "source", "derivative", "root"), prefix = NULL, relative_to_project = FALSE) {

  docstring <- 'Usage -> bidsr::resolve_bids_path(
    x, ...,
    storage = c("raw", "source", "derivative", "root"),
    prefix = NULL,
    relative_to_project = FALSE
)

  x: BIDS subjecr object; see `?bids_subject`;

  storage: which storage to return, choices are
    - "root": project root
    - "raw" : subject folder under the raw-data directory
    - "source": subject folder under the source-data path
    - "derivative": subject folder under the derivative path

  prefix: only used when `storage="derivative"`, prefix of the derivative name

  relative_to_project: whether to return relative path to the project root;
    default is `FALSE`

  ...: additional paths to append to return.

Examples:

  # Path to rawdata -> sub-01
  resolve_bids_path(subject, storage = "raw")
  #> /BIDS/ieeg_epilepsy_ecog/sub-01

  # Path to freesurfer derivatives -> sub-01
  resolve_bids_path(subject, storage = "derivative",
                    prefix = "freesurfer")
  #> /BIDS/ieeg_epilepsy_ecog/derivatives/freesurfer/sub-01

Returns: a resolved path.
  '
  use_docstring({
    storage <- match.arg(storage)
    pardir <- resolve_bids_path(x@project, storage = storage, relative_to_project = relative_to_project)
  }, docstring)

  # project root
  if( storage == "root" ) { return(pardir) }
  if(!nzchar(pardir) || trimws(pardir) %in% c(".", "")) {
    pardir <- NULL
  }

  path_join(c(pardir, prefix, sprintf("sub-%s", x@subject_code)))
}


## `query_bids` generic
S7::method(query_bids, bids_subject) <- function(x, search_params, ..., env = parent.frame()) {


  docstring <- "Usage -> bidsr::query_bids(subject, search_params, ...)

  subject: BIDS subject (see `?bids_subject`)
  search_params: query paramenters, can be a string or a list:
    - [type=string] data types to query in raw folder, e.g. 'anat', 'func', ...
    - [type=list] list of data type, storage, ...
        list(
          storage     = [default='raw'] storage path to query, choices
                          are 'raw', 'source', or 'derivative'
          prefix      = [default=NULL] prefix (software) when searching
                          derivatives (storage='derivative')
          suffixes    = [default=NULL] suffixes of the files; set to `NULL`
                          to disable this filter
          data_types  = [default=NULL] data type to query in raw folder,
                          e.g. 'anat', 'func', ...; set it to `NULL` to
                          query all data types
          sidecars    = [default=FALSE] whether to include `JSON` sidecars;
          entity_filters = [default=list()] a list of filters in formula
                          format; See `vignette(package = 'bidsr')` for
                          examples.
        )

Examples:

  filter_result <- query_bids(subject, list(
    storage = 'raw',
    sidecars = FALSE,
    data_types = 'func',
    suffixes = 'events',

    entity_filters = list(
      run ~ as.integer(run) == 2,
      acq ~ acq %in% c('drawing02', 'drawing03')
    )
  ))

Returns: a data table of files and entities related to the data type.
  "
  use_docstring(search_params, docstring)

  # DIPSAUS DEBUG START
  # examples <- download_bids_examples(test = TRUE)
  # project <- bids_project("/Users/dipterix/Library/Caches/org.R-project.R/R/bidsr/bids-examples/bids-examples-master/ieeg_epilepsy_ecog/")
  # subject_code <- "ecog01"
  #
  # project <- "/Users/dipterix/Library/Caches/org.R-project.R/R/bidsr/bids-examples/bids-examples-master/ds000117"
  # subject_code <- "06"
  # x <- bids_subject(project = project, subject_code = subject_code)
  # ii <- 1
  # search_params <- list(
  #   storage = "raw",
  #   sidecars = FALSE,
  #
  #   data_types = "func",
  #   suffixes = "events",
  #
  #   # use R "formula" to filter entities
  #   entity_filters = list(
  #
  #     # entity_key ~ expression returning TRUE/FALSE
  #     # When filtering the entities, `entity_key` will be
  #     # replaced with its value
  #     run ~ as.integer(run) == 2
  #   )
  # )
  # env <- parent.frame()

  force(env)
  if(is.list(search_params)) {
    data_types <- search_params$data_types
    storage <- match.arg(search_params$storage, c("raw", "source", "derivative"))
    prefix <- search_params$prefix
    sidecars <- isTRUE(search_params$sidecars)
    suffixes <- search_params$suffixes
    entity_filters <- search_params$entity_filters
    if(!is.list(entity_filters)) {
      if(inherits(entity_filters, "formula")) {
        entity_filters <- list(entity_filters)
      } else {
        entity_filters <- stats::as.formula(entity_filters, env = env)
      }
    }
  } else {
    data_types <- search_params
    storage <- "raw"
    prefix <- NULL
    sidecars <- FALSE
    suffixes <- NULL
    entity_filters <- list()
  }
  suffixes <- tolower(as.character(suffixes))

  project_root <- resolve_bids_path(x, storage = "root")

  subject_root <- resolve_bids_path(x, storage = storage, prefix = prefix, relative_to_project = TRUE)
  subject_absroot <- file_path(project_root, subject_root)

  if(!dir_exists(subject_absroot)) {
    stop(sprintf("Cannot find directory for subject [sub-%s]: %s", x@subject_code, subject_absroot))
  }
  project_root <- path_abs(project_root)
  subject_absroot <- path_abs(subject_absroot)

  storage_root <- dirname(subject_root)
  if(!storage_root %in% c("", ".")) {
    storage_absroot <- file_path(project_root, storage_root)
  } else {
    storage_absroot <- project_root
  }

  # query files
  # in the root,
  if(length(data_types)) {
    data_types <- as.character(unname(unlist(data_types)))
    data_types <- unique(data_types)
  }
  # 1 ses- folder
  level1_files <- list.files(subject_absroot, include.dirs = TRUE, recursive = FALSE, full.names = FALSE, all.files = FALSE)
  level1_dirnames <- level1_files[dir_exists(file_path(subject_absroot, level1_files))]

  is_ses_dirs <- startsWith(tolower(level1_dirnames), "ses-")
  ses_dirs <- level1_dirnames[is_ses_dirs]
  level1_dirnames <- level1_dirnames[!is_ses_dirs]
  if(length(data_types)) {
    level1_dirnames <- level1_dirnames[level1_dirnames %in% data_types]
  }


  # 2 datatype folder
  level2_dirnames <- lapply(ses_dirs, function(ses_dir) {
    if(length(data_types)) {
      sub_dirnames <- list.files(
        path = file_path(subject_absroot, ses_dir),
        all.files = FALSE,
        pattern = sprintf("^%s$", paste(data_types, collapse = "|")),
        ignore.case = TRUE,
        recursive = FALSE,
        include.dirs = TRUE,
        full.names = FALSE
      )
    } else {
      sub_dirnames <- list.files(
        path = file_path(subject_absroot, ses_dir),
        all.files = FALSE,
        recursive = FALSE,
        include.dirs = TRUE,
        full.names = FALSE
      )
    }
    file_path(ses_dir, sub_dirnames)
  })

  level1_dirnames <- unique(unlist(level1_dirnames))
  level2_dirnames <- unique(unlist(level2_dirnames))
  data_type_paths <- c(level1_dirnames, level2_dirnames)
  # data_type_paths <- list.files(
  #   path = subject_absroot,
  #   all.files = FALSE,
  #   pattern = sprintf("^%s$", data_type),
  #   recursive = TRUE,
  #   include.dirs = TRUE,
  #   full.names = FALSE
  # )
  if(length(data_type_paths)) {
    data_type_paths <- file_path(subject_root, data_type_paths)
    if(length(level2_dirnames)) {
      level2_dirnames <- file_path(subject_root, level2_dirnames)
      level2_dirnames <- path_expand(level2_dirnames)
    }
  } else {
    data_type_paths <- subject_root
  }

  # Initialize: find all the folders to query
  search_paths <- lapply(strsplit(data_type_paths, "[/|\\\\]+"), function(path_split) {
    path_split <- path_split[!path_split %in% c("", ".")]
    Reduce(
      init = ".", x = path_split,
      right = FALSE,
      function(re, name) {
        c(file_path(re[[1]], name), re)
      }
    )
  })
  search_paths <- sort(unique(unlist(search_paths)))
  search_paths <- path_expand(search_paths)

  # Iterate find all associated files
  associated_files <- lapply(search_paths, function(relpath) {
    abspath <- file_path(project_root, relpath)
    lowest_level <- relpath %in% level2_dirnames

    if( lowest_level ) {
      filenames <- list.files(path = abspath, all = FALSE, full.names = FALSE, include.dirs = TRUE, recursive = FALSE)
    } else {
      filenames <- list_files_only(path = abspath, all = FALSE, full_names = FALSE)
    }

    list(
      relpath = file_path(relpath, filenames),
      lowest_level = rep(lowest_level, length(filenames))
    )
  })
  associated_files <- data.table::rbindlist(associated_files)
  associated_files <- unique(associated_files)

  file_parsed <- lapply(seq_len(nrow(associated_files)), function(ii) {
    path <- associated_files$relpath[[ii]]
    lowlevel <- associated_files$lowest_level[[ii]]
    tryCatch({
      parsed <- parse_path_bids_entity(path)
      if(lowlevel || length(names(parsed@entities))) {
        return(parsed)
      }
    }, error = function(e) {
      NULL
    })
    return(NULL)
  })

  # get data files that passed filters
  datafile_pass_filters <- vapply(file_parsed, FUN.VALUE = FALSE, function(parsed) {
    if(is.null(parsed)) { return(FALSE) }
    if(isTRUE(tolower(parsed@extension) %in% c("tsv", "json", "tsv.gz", "bvec", "bval"))) { return(FALSE) }
    if(length(suffixes) && !isTRUE(tolower(parsed@suffix) %in% suffixes)) { return(FALSE) }
    if(length(entity_filters)) {
      if(!test_bids_entities(parsed, .rules = entity_filters, envir = env)) {
        return(FALSE)
      }
    }
    return(TRUE)
  })
  datafiles <- file_parsed[datafile_pass_filters]

  # get common entities
  entity_names <- lapply(datafiles, function(item) {
    names(item@entities)
  })
  entity_names <- unique(unlist(entity_names))

  # get entities
  entity_values <- new.env(parent = emptyenv())
  list2env(structure(names = entity_names, rep(list(NULL), length(entity_names))), envir = entity_values)
  if(length(entity_names)) {
    lapply(datafiles, function(item) {
      lapply(entity_names, function(enm) {
        e <- item@entities[[enm]]
        if(S7::S7_inherits(e, bids_entity)) {
          entity_values[[enm]] <- c(entity_values[[enm]], e@value)
        }
        NULL
      })
      NULL
    })
  }

  entity_values <- structure(
    names = entity_names,
    lapply(entity_names, function(enm) {
      unique(entity_values[[enm]])
    })
  )

  # now let's filter meta
  meta_pass_filters <- vapply(file_parsed, FUN.VALUE = FALSE, function(parsed) {
    if(is.null(parsed)) { return(FALSE) }
    extension <- tolower(parsed@extension)

    if(!sidecars && identical( extension , "json" )) { return(FALSE) }
    if(!isTRUE(extension %in% c("tsv", "json", "tsv.gz", "bvec", "bval"))) { return(FALSE) }

    if(length(suffixes) && !isTRUE(tolower(parsed@suffix) %in% suffixes)) { return(FALSE) }

    # if(length(entity_names)) {
    #   meta_entity_names <- names(parsed@entities)
    #   if(!all(meta_entity_names %in% entity_names)) { return(FALSE) }
    #
    #   for(enm in meta_entity_names) {
    #     # also need to in the values
    #     if( !isTRUE(parsed@entities[[enm]]@value %in% entity_values[[enm]]) ) { return( FALSE ) }
    #   }
    # } else {
      # no datafile is found, maybe user is looking for meta files???
      if(!test_bids_entities(parsed, .rules = entity_filters, envir = env)) {
        return(FALSE)
      }
    # }

    return(TRUE)
  })

  pass_filters <- meta_pass_filters | datafile_pass_filters
  associated_files <- associated_files[pass_filters, ]
  file_parsed <- file_parsed[pass_filters]

  entity_names <- lapply(file_parsed, function(item) {
    names(item@entities)
  })
  entity_names <- unique(unlist(entity_names))

  query_result <- lapply(seq_len(nrow(associated_files)), function(ii) {
    path <- associated_files$relpath[[ii]]
    item <- file_parsed[[ii]]
    c(
      list(
        parsed = I(list(item)),
        # path = file_path
        data_type = item@data_type,
        suffix = paste(item@suffix, collapse = "_"),
        extension = paste(item@extension, collapse = ".")
      ),
      structure(
        names = entity_names,
        lapply(entity_names, function(nm) {
          get_bids_entity(item, nm, value_only = TRUE, ifnotfound = NA)
        })
      )
    )
  })

  query_result <- data.table::rbindlist(query_result)
  return(structure(
    query_result,
    project_root = project_root,
    subject = x
  ))
}

