#' @title 'BIDS' project class
#' @author Zhengjia Wang
#' @name bids_project
#' @param path absolute path to the 'BIDS' project directory;
#' @param raw_data_relpath raw data-set path, relative (to the \code{path});
#' @param source_data_relpath source data-set path, relative (to the \code{path});
#' @param derivative_data_relpath derivative data-set path, relative (to the \code{path});
#' @param strict whether \code{path} needs to exist; default is \code{TRUE}
#' @returns A 'BIDS' project instance.
#'
#' @examples
#'
#'
#' # Run `download_bids_examples()` first
#' examples <- download_bids_examples(test = TRUE)
#' if(!isFALSE(examples)) {
#'
#'   project_path <- file.path(examples, "ieeg_epilepsy_ecog")
#'
#'   project <- BIDSProject(
#'     path = project_path,
#'     raw_data_relpath = ".",
#'     derivative_data_relpath = "derivatives"
#'   )
#'
#'   project
#'
#' }
#'
#'
#'
#' @export
BIDSProject <- new_bids_class(
  name = "BIDSProject",
  properties = list(
    name = bids_property_character(
      name = "name",
      getter = function(self) {
        return(basename(self@path))
      }
    ),
    path = bids_property_character(
      name = "path",
      type = "required",
      validator = function(value) {
        if (length(value) != 1) {
          return("BIDS project path must be a length of 1")
        }
        if (is.na(value) || !is.character(value)) {
          return("BIDS project path must be string and cannot be NA")
        }
        if (trimws(value) %in% c("", "/")) {
          return("BIDS project path must be a valid path and cannot be blank nor `/`")
        }
        if(is_file(value)) {
          return("Path exists but appear to be a file. BIDS project path must be a directory.")
        }
        return(NULL)
      },
      setter = function(self, value) {
        if(length(value) != 1) {
          stop("BIDS project path must be a length of 1")
        }
        value_ <- tolower(value)
        using_protocol <- grepl("^[a-z]+://", value_) || startsWith(value_, "doi:")
        if(!isTRUE(using_protocol)) {
          value <- path_abs(value)
        }
        self@path <- value
        self
      }
    ),
    raw_data_relpath = bids_property_character(
      name = "raw_data_relpath",
      type = "required",
      default = "."
    ),
    source_data_relpath = bids_property_character(
      name = "source_data_relpath",
      type = "required",
      default = "sourcedata"
    ),
    derivative_data_relpath = bids_property_character(
      name = "derivative_data_relpath",
      type = "required",
      default = "derivatives"
    ),
    README = bids_property_collapsed_character(
      name = "README",
      type = "required",
      collapse = "\n"
    )
  ),
  constructor = function(path, raw_data_relpath = ".", source_data_relpath = "sourcedata", derivative_data_relpath = "derivatives", strict = TRUE) {
    path <- normalizePath(path = path, mustWork = strict, winslash = "/")
    readme_path <- file_path(path, "README")
    readme <- "(No README)"
    if(file_exists(readme_path)) {
      suppressWarnings({
        try({
          readme <- readLines(readme_path)
        })
      })
    }
    S7::new_object(
      S7::S7_object(),
      path = path,
      raw_data_relpath = raw_data_relpath,
      source_data_relpath = source_data_relpath,
      derivative_data_relpath = derivative_data_relpath,
      README = readme
    )
  }
)

#' @rdname bids_project
#' @export
bids_project <- function (path, raw_data_relpath = ".", source_data_relpath = "sourcedata",
                          derivative_data_relpath = "derivatives", strict = TRUE) {
  BIDSProject(
    path = path,
    raw_data_relpath = raw_data_relpath,
    source_data_relpath = source_data_relpath,
    derivative_data_relpath = derivative_data_relpath,
    strict = strict
  )
}

# helper to get path to project
path_to_project <- function(x, relpath, storage = "root", check = TRUE) {
  x_path <- format(x, storage = storage)

  ds_path <- path_join(c(x_path, relpath))
  if(check && !file_exists(ds_path)) {
    stop(
      sprintf(
        "Cannot find `%s` under the project folder. Please specify the **relative** `parent_directory` explicitly.",
        relpath
      )
    )
  }

  return(ds_path)
}

## `format` generic
S7::method(format.generic, BIDSProject) <- function(x, storage = c("root", "raw", "source", "derivative"), ...) {
  storage <- match.arg(storage)
  path <- switch(
    storage,
    "root" = x@path,
    "raw" = path_join(c(x@path, x@raw_data_relpath)),
    "source" = path_join(c(x@path, x@source_data_relpath)),
    "derivative" = path_join(c(x@path, x@derivative_data_relpath))
  )
  path
}

## `print` generic
S7::method(print.generic, BIDSProject) <- function(x, width = getOption("width", 80L), ...) {
  screen_width <- max(20L, min(width, 80L))
  cat(
    sep = "\n",
    c(
      sprintf("<%s>[BIDSProject] at:", x@name),
      sprintf("  %s", format(x)),
      sprintf("  - raw-data path: %s", x@raw_data_relpath),
      sprintf("  - source-data path: %s", x@source_data_relpath),
      sprintf("  - derivative path: %s", x@derivative_data_relpath),
      sprintf("+- Descriptions: %s", paste(rep("-", screen_width - 17L), collapse = "")),
      strwrap(x@README, prefix = "| ", width = screen_width - 2L)
    )
  )
  invisible(x)
}

## `[[` generic
TOP_LEVEL_GETTERS <- c("get_bids_dataset_description", "get_bids_participants", "get_bids_samples", "get_bids_phenotype_data")
S7::method(extract_bracket.generic, list(x = BIDSProject, name = S7::class_any)) <- function(x, name, ...) {
  if(name %in% TOP_LEVEL_GETTERS) {
    re <- switch (
      name,
      "get_bids_phenotype_data" = function(measurement_tool_name, ..., test = FALSE) {
        args <- list(x = x,
                     measurement_tool_name = measurement_tool_name,
                     ...,
                     test = test)
        do.call(name, args)
      },
      {
        function(..., test = FALSE) {
          args <- list(x = x,
                       ...,
                       test = test)
          do.call(name, args)
        }
      }
    )
    return(re)
  }
  re <- extract_bids_class_base(x, name)
  return(re)
}

## `names` generic
S7::method(names.generic, BIDSProject) <- function(x) {
  return(unique(
    c(
      names_bids_class_base(x),
      TOP_LEVEL_GETTERS
    )
  ))
}

## `resolve_bids_path` generic
S7::method(resolve_bids_path, BIDSProject) <- function(x, ..., storage = c("root", "raw", "source", "derivative"), relative_to_project = FALSE) {

  docstring <- 'Usage -> bidsr::resolve_bids_path(
    x, ...,
    storage = c("root", "raw", "source", "derivative"),
    relative_to_project = FALSE
)

  x: BIDS project object; see `?BIDSProject`;

  storage: which storage to return, choices are
    - "root": project root
    - "raw" : raw-data directory (often the same as root but not necessary)
    - "source": source-data path (often sourcedata/)
    - "derivative": derivative path (often derivatives/)

  relative_to_project: whether to return relative path to the project root;
    default is `FALSE`

  ...: additional paths to append to return.

Examples:

  # Path to rawdata -> sub-01
  resolve_bids_path(project, "sub-01", storage = "raw",
                    relative_to_project = TRUE)
  #> ./sub-01


Returns: a resolved path.
  '
  use_docstring({
    storage <- match.arg(storage)
  }, docstring)


  relpath <- switch(
    storage,
    "root" = ".",
    "raw" = c(x@raw_data_relpath),
    "source" = c(x@source_data_relpath),
    "derivative" = c(x@derivative_data_relpath)
  )
  if( relative_to_project ) {
    ds_path <- path_join(c(relpath, ...))
  } else {
    ds_path <- path_join(c(x@path, relpath, ...))
  }

  ds_path
}


# ---- get top-level files

## `get_bids_dataset_description` generic
S7::method(get_bids_dataset_description, BIDSProject) <- function(
    x, parent_directory, storage = c("root", "raw", "source", "derivative"), test = FALSE, ...) {

  storage <- match.arg(storage)
  if(missing(parent_directory)) {
    parent_directory <- NULL
  }
  ds_path <- path_to_project(
    x = x, storage = storage,
    relpath = path_join(c(parent_directory, "dataset_description.json")),
    check = !test
  )
  if(test) {
    return(structure(
      file_exists(ds_path),
      names = ds_path
    ))
  }
  get_bids_dataset_description(x = ds_path)
}

## `get_bids_participants` generic
S7::method(get_bids_participants, BIDSProject) <- function(
    x, parent_directory = NULL, storage = c("root", "raw", "source", "derivative"), test = FALSE, ...) {
  storage <- match.arg(storage)
  ds_path <- path_to_project(
    x = x, storage = storage,
    relpath = path_join(c(parent_directory, "participants.tsv")),
    check = !test
  )
  if(test) {
    return(structure(
      file_exists(ds_path),
      names = ds_path
    ))
  }
  get_bids_participants(x = ds_path)
}

## `get_bids_samples` generic
S7::method(get_bids_samples, BIDSProject) <- function(
    x, parent_directory = NULL, storage = c("root", "raw", "source", "derivative"), test = FALSE, ...) {
  storage <- match.arg(storage)
  ds_path <- path_to_project(
    x = x, storage = storage,
    relpath = path_join(c(parent_directory, "samples.tsv")),
    check = !test
  )
  if(test) {
    return(structure(
      file_exists(ds_path),
      names = ds_path
    ))
  }
  get_bids_samples(x = ds_path)
}

## `get_bids_phenotype_data` generic
S7::method(get_bids_phenotype_data, BIDSProject) <- function(
    x, measurement_tool_name, parent_directory = "phenotype",
    storage = c("root", "raw", "source", "derivative"), test = FALSE, ...) {

  storage <- match.arg(storage)

  if(!endsWith(tolower(measurement_tool_name, "tsv"))) {
    measurement_tool_name <- gsub("\\.(tsv|json)$", "", measurement_tool_name, ignore.case = TRUE)
    measurement_tool_name <- sprintf("%s.tsv", measurement_tool_name)
  }

  ds_path <- path_to_project(
    x = x, storage = storage,
    relpath = path_join(c(parent_directory, measurement_tool_name)),
    check = !test
  )
  if(test) {
    return(structure(
      file_exists(ds_path),
      names = ds_path
    ))
  }
  get_bids_phenotype_data(x = ds_path)
}
