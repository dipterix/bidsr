#' @title 'BIDS' project class
#' @name bids_project
#' @param path absolute path to the 'BIDS' project directory;
#' @param raw_data_relpath raw data-set path, relative (to the \code{path});
#' @param source_data_relpath source data-set path, relative (to the \code{path});
#' @param derivative_data_relpath derivative data-set path, relative (to the \code{path});
#' @returns A 'BIDS' project instance
#'
#' @examples
#'
#'
#' example_path <- system.file(
#'   "examples", "ieeg_epilepsy_ecog", package = "bidsr")
#'
#' project <- bids_project(
#'   path = example_path,
#'   raw_data_relpath = ".",
#'   derivative_data_relpath = "derivatives"
#' )
#'
#'
#'
#' @export
bids_project <- new_bids_class(
  name = "bids_project",
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
  },
  methods = list(
    format = function(self, storage = c("root", "raw", "source", "derivative"), ...) {
      storage <- match.arg(storage)
      path <- switch(
        storage,
        "root" = self@path,
        "raw" = path_join(c(self@path, self@raw_data_relpath)),
        "source" = path_join(c(self@path, self@source_data_relpath)),
        "derivative" = path_join(c(self@path, self@derivative_data_relpath))
      )
      path
    },
    print = function(self, ...) {
      screen_width <- max(20L, min(getOption("width"), 80L))
      cat(
        sep = "\n",
        c(
          sprintf("<%s>[bids_project] at:", self@name),
          sprintf("  %s", format(self)),
          sprintf("  - raw-data path: %s", self@raw_data_relpath),
          sprintf("  - source-data path: %s", self@source_data_relpath),
          sprintf("  - derivative path: %s", self@derivative_data_relpath),
          sprintf("+- Descriptions: %s", paste(rep("-", max(screen_width - 17L, 20L)), collapse = "")),
          strwrap(self@README, prefix = "| ", width = max(screen_width - 2L, 20L))
        )
      )
    },
    get_dataset_description = function(self, relpath = "dataset_description.json") {
      ds_path <- file_path(self, relpath)
      if(!file_exists(ds_path)) {
        stop(sprintf("Cannot find `%s` under the project folder. Please specify the path explicitly.", relpath))
      }
      as_bids_dataset_description(x = ds_path)
    },
    get_participants = function(self, relpath = "participants.tsv") {
      ds_path <- file_path(self, relpath)
      if(!file_exists(ds_path)) {
        stop(sprintf("Cannot find `%s` under the project folder. Please specify the path explicitly.", relpath))
      }
      as_bids_tabular(x = ds_path)
    }
  )
)

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
#' example_path <- system.file(
#'   "examples", "ieeg_epilepsy_ecog", package = "bidsr")
#'
#' project <- bids_project(
#'   path = example_path,
#'   raw_data_relpath = ".",
#'   derivative_data_relpath = "derivatives"
#' )
#'
#' subject <- bids_subject(project = project, subject_code = "ecog01",
#'                         strict = FALSE)
#'
#' storage_root <- subject$get_path("raw")
#'
#' if( dir.exists(storage_root) ) {
#'   subject$query_modality("ieeg")
#' }
#'
#' @export
bids_subject <- new_bids_class(
  name = "bids_subject",
  # hidden_names = c(".prepare_save"),
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
  },
  methods = list(
    get_path = function(self, storage = c("raw", "source", "derivative"), derivative_prefix = NULL) {
      storage <- match.arg(storage)
      relpath <- S7::prop(self@project, sprintf("%s_data_relpath", storage))

      if(storage == "derivative") {
        additional_prefix <- as.character(derivative_prefix)
        additional_prefix <- additional_prefix[!is.na(additional_prefix)]
      } else {
        additional_prefix <- NULL
      }
      if(!nzchar(relpath) || trimws(relpath) %in% c(".", "")) {
        relpath <- NULL
      }
      storage_root <- path_join(c(self$project$path, relpath, additional_prefix, sprintf("sub-%s", self$subject_code)))
      storage_root
    },
    print = function(self, ...) {
      cat(sprintf("<BIDS Subject> `sub-%s` (project `%s`)\n", self@subject_code, self@project@name))
    },
    query_modality = function(
      self, modality, storage = c("raw", "source", "derivative"),
      ..., derivative_prefix = NULL) {

      storage <- match.arg(storage)

      # DIPSAUS DEBUG START
      # project <- bids_project("/Users/dipterix/rave_data/bids_dir/NSDiEEG_PennShare")
      # self <- bids_subject(project = project, subject_code = "06")
      # storage <- "raw"
      # additional_prefix <- NULL
      # modality <- "ieeg"
      # ii <- 1
      # self$query_modality(modality = "ieeg")

      if(!isTRUE(nzchar(modality))) {
        stop("BIDS modality cannot be empty.")
      }
      storage_root <- self$get_path(storage = storage, derivative_prefix = derivative_prefix)

      if(!dir_exists(storage_root)) {
        stop(sprintf("Cannot find directory for subject [sub-%s]: %s", self$subject_code, storage_root))
      }

      # query files
      # according to
      # https://bids-specification.readthedocs.io/en/stable/common-principles.html#the-inheritance-principle
      # There are 4 common paths

      modality_paths <- list.files(
        path = storage_root,
        all.files = FALSE, pattern = sprintf("^%s$", modality),
        recursive = TRUE,
        include.dirs = TRUE,
        full.names = FALSE
      )

      # Example 1: immediately under subject folder
      associated_files_0 <- list_files_only(path = dirname(storage_root), all = FALSE, full_names = FALSE)
      associated_files_1 <- list_files_only(path = storage_root, all = FALSE, full_names = FALSE)


      associated_files_2 <- lapply(modality_paths, function(modality_path) {
        # modality_path <- modality_paths[[1]]
        modality_pathsplit <- strsplit(modality_path, "[/|\\\\]")[[1]]
        modality_root1 <- file_path(storage_root, modality_pathsplit[[1]])
        associated_subfiles_1 <- file_path(modality_pathsplit[[1]], list_files_only(modality_root1))

        if(length(modality_pathsplit) > 1) {
          modality_root2 <- file_path(storage_root, modality_path)
          associated_subfiles_2 <- file_path(modality_path, list.files(modality_root2, all.files = FALSE, full.names = FALSE, recursive = FALSE, include.dirs = TRUE, no.. = TRUE))
        } else {
          associated_subfiles_2 <- NULL
        }

        c(associated_subfiles_1, associated_subfiles_2)
      })

      associated_files <- unique(c(associated_files_0, associated_files_1, unlist(associated_files_2)))

      file_parsed <- lapply(associated_files, function(path) {
        parse_path_bids_entity(path)
      })

      entity_names <- lapply(file_parsed, function(item) {
        names(item@entities)
      })
      has_entities <- vapply(entity_names, function(nms) { length(nms) > 0 }, FALSE)
      entity_names <- unique(unlist(entity_names))

      associated_files <- associated_files[has_entities]
      file_parsed <- file_parsed[has_entities]

      query_result <- lapply(seq_along(associated_files), function(ii) {
        path <- associated_files[[ii]]
        item <- file_parsed[[ii]]
        c(
          list(
            parsed = I(list(item)),
            data_type = item@data_type,
            suffix = paste(item@suffix, collapse = "_"),
            extension = paste(item@extension, collapse = ".")
          ),
          structure(
            names = entity_names,
            lapply(entity_names, function(nm) {
              item$get_entity(nm, value_only = TRUE, ifnotfound = NA)
            })
          )
        )
      })

      query_result <- data.table::rbindlist(query_result)
      structure(
        query_result,
        root_path = storage_root,
        subject = self
      )
    }
  )
)
