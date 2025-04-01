# https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html
# Modality agnostic files


#' Class definition for 'BIDS' meta-data \code{'GeneratedBy'}
#' @author Zhengjia Wang
#' @description
#' See definition at \url{https://bids-specification.readthedocs.io/en/stable/glossary.html#objects.metadata.GeneratedBy}
#' @param Name (character, required) name of the pipeline or process that
#' generated the outputs.
#' @param Version (character, optional) version of the pipeline
#' @param Description (character, optional) plain-text description of the pipeline or process that generated the outputs.
#' @param CodeURL (character, optional) 'URL' where the code used to generate
#' the data may be found.
#' @param Container (character, optional) Used to specify the location and
#' relevant attributes of software container image used to produce the data.
#' Valid keys in this object include type, tag 'URL' with string values.
#' Package \code{'bidsr'} does not check what's inside of this entry.
#' @returns Instantiated object of class \code{BIDSDatasetGeneratedBy}
#' @examples
#'
#' x <- BIDSDatasetGeneratedBy(
#'   Name = "RAVE Team",
#'   Version = "0.0.1",
#'   Container = list(
#'     Type = "docker",
#'     Tag = "rave-ieeg/rave-pipelines:0.0.1"
#'   )
#' )
#'
#' x
#'
#' x$Version <- "0.0.2"
#'
#' # convert to basic list
#' as.list(x)
#'
#' # get JSON string
#' format(x)
#'
#' @export
BIDSDatasetGeneratedBy <- new_bids_class(
  name = "BIDSDatasetGeneratedBy",
  properties = list(
    Name = bids_property_character(name = "Name", type = "required", validator = validator_nonempty_string),
    Version = bids_property_character(name = "Version", type = "optional"),
    Description = bids_property_character(name = "Description", type = "optional"),
    CodeURL = bids_property_character(name = "CodeURL", type = "optional"),
    Container = bids_property_named_list(name = "Container")
  )
)

## `format`
S7::method(format.generic, BIDSDatasetGeneratedBy) <- function(x, ..., indent = json_indent()) {
  to_json(as.list(x, recursive = TRUE), indent = indent)
}

#' Class definition of 'BIDS' data-set description
#' @author Zhengjia Wang
#' @description
#' See \url{https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#dataset_descriptionjson}
#' for specification.
#'
#' @param x R object to be interpreted as 'BIDS' data description; default
#' support list, path to the \code{'json'} file, \code{'json'} string, etc.
#' @param parent_directory parent directory where the file
#' \code{'dataset_description.json'} is stored. This input is ignored
#' if \code{x} is the path to \code{'dataset_description.json'}, otherwise
#' is a must.
#' @param ... passed to methods
#' @param Name (required, string) Name of the data-set.
#' @param BIDSVersion (required, string) The version of the BIDS standard that
#' was used.
#' @param DatasetLinks (required if 'BIDS-URI' is used) Used to map a given
#' data-set name from a 'BIDS-URI' of the form
#' \code{bids:<dataset-name>:path/within/dataset} to a local or remote location.
#' @param HEDVersion (recommended strings) The version of the 'HED' schema
#' used to validate 'HED' tags for study. May include a single schema or a
#' base schema and one or more library schema.
#' @param DatasetType (recommended string) Must be one of \code{"raw"} or
#' \code{"derivative"}; package \code{bidsr} automatically assigns
#' \code{"raw"} is not given.
#' @param License (recommended string) The license for the data-set
#' @param Authors (recommended strings) Vector of individuals who contributed
#' to the creation/curation of the data-set
#' @param GeneratedBy (recommended) will be converted to
#' \code{\link{BIDSDatasetGeneratedBy}}
#' @param SourceDatasets Used to specify the locations and relevant
#' attributes of all source data-sets. Valid keys in each object include
#' \code{"URL"}, \code{"DOI"}, and \code{"Version"} with string values;
#' Package \code{bidsr} does not check the names
#' @param Acknowledgements (optional string) Text acknowledging contributions
#' of individuals or institutions beyond those listed in Authors or Funding.
#' @param HowToAcknowledge (optional string) Text containing instructions on
#' how researchers using this dataset should acknowledge the original authors.
#' This field can also be used to define a publication that should be cited
#' in publications that use the dataset.
#' @param Funding (optional strings) List of sources of funding (grant numbers).
#' @param EthicsApprovals (optional strings) List of ethics committee
#' approvals of the research protocols and/or protocol identifiers.
#' @param ReferencesAndLinks (optional strings) List of references to
#' publications that contain information on the data-set. A reference may be
#' textual or a URI.
#' @param DatasetDOI (optional string) The Digital Object Identifier of the
#' data-set (not the corresponding paper). \code{'DOIs'} should be expressed
#' as a valid 'URI'
#' @returns A \code{S7} description object that contains all the fields
#' describing the data set; see 'Examples' for usages.
#'
#' @examples
#'
#' # ---- Manually enter entries ----------------------------------------
#' dataset_description <- BIDSDatasetDescription(
#'   # a parent directory is mandatory as it defines what data
#'   # dataset_description.json applies to
#'
#'   parent_directory = "/path/to/BIDS/folder",
#'
#'   Name = "A dummy experiments",
#'   BIDSVersion = "1.6.0",
#'   License = "CC0",
#'   Authors = c("Zhengjia Wang"),
#'   Acknowledgements = c(
#'     "Package `bidsr` is a 3rd-party BIDS reader developed by",
#'     "a RAVE (https://rave.wiki) team member with procrastination."
#'   ),
#'   HowToAcknowledge = c(
#'     "Please cite this paper:",
#'     "https://doi.org/10.1016/j.neuroimage.2020.117341"
#'   ),
#'   Funding = c(
#'     "NIH R01MH133717",
#'     "NIH U01NS113339",
#'     "NIH 1R24MH117529"
#'   ),
#'   ReferencesAndLinks = c(
#'     "https://rave.wiki"
#'   ),
#'   DatasetDOI = "https://doi.org/10.1016/j.neuroimage.2020.117341",
#'   HEDVersion = "8.0.0",
#'   GeneratedBy = list(
#'     list(
#'       Name = "Dipterix",
#'       Version = "0.0.1",
#'       Container = list(
#'         Type = "r-package",
#'         Tag = "dipterix/bidsr:0.0.1"
#'       )
#'     )
#'   )
#' )
#'
#' # access the information
#' dataset_description$License
#'
#' dataset_description$GeneratedBy[[1]]$Container
#'
#' # ---- Read from file ---------------------------------------------
#'
#' # Run `download_bids_examples()` first
#' examples <- download_bids_examples(test = TRUE)
#' if(!isFALSE(examples)) {
#'   example_descr <- file.path(
#'     examples, "ieeg_epilepsy_ecog", "dataset_description.json")
#'
#'   x <- get_bids_dataset_description(example_descr)
#'   x
#'
#'   # ---- Formatting --------------------------------------------------
#'   # convert to R list (use recursive to expand field `GeneratedBy`)
#'   as.list(x, recursive = TRUE)
#'
#'   # JSON string
#'   format(x)
#' }
#'
#' @export
BIDSDatasetDescription <- new_bids_class(
  name = "BIDSDatasetDescription",
  properties = list(
    # --- Required ---
    Name = bids_property_character(name = "Name", type = "required", validator = validator_nonempty_string),
    BIDSVersion = bids_property_character(name = "BIDSVersion", type = "required", validator = validator_nonempty_string),
    # --- Conditionally required ---
    # FIXME: REQUIRED if BIDS URIs are used.
    # It's also object of strings (what's that???)
    DatasetLinks = bids_property_named_list(name = "DatasetLinks"),

    # --- Recommended ---
    # array of strings
    HEDVersion = bids_property_character(name = "HEDVersion", type = "optional"),

    DatasetType = bids_property_character(
      name = "DatasetType",
      setter = function(self, value) {
        if(!length(value) || missing(value) || isTRUE(is.na(value))) {
          value <- "raw"
        } else {
          value <- match.arg(value, choices = c("raw", "derivative"))
        }
        self@DatasetType <- value
        self
      },
      validator = function(value) {
        if(length(value) != 1) { return() }
        if( value %in% c("raw", "derivative") ) { return() }
        return("DatasetType must be raw or derivative")
      }
    ),

    License = bids_property_character(name = "License", type = "optional", max_len = NULL),

    Authors = bids_property_character(name = "Authors", type = "optional", max_len = NULL),

    # array of objects
    GeneratedBy = bids_property_unnamed_list(
      name = "GeneratedBy",
      setter = function(self, value) {
        self@GeneratedBy <- structure(
          names = NULL,
          lapply(value, function(x) {
            if(!S7::S7_inherits(x, class = BIDSDatasetGeneratedBy)) {
              x <- do.call(BIDSDatasetGeneratedBy, as.list(x))
            }
            x
          })
        )
        self
      }
    ),
    SourceDatasets = bids_property_unnamed_list(
      name = "SourceDatasets",
      setter = function(self, value) {
        self@SourceDatasets <- unname(value)
        self
      }
    ),

    # --- Optional ---
    # Might be too long, using make_collapsed_string_property
    Acknowledgements = bids_property_collapsed_character(
      name = "Acknowledgements", type = "optional", collapse = " "
    ),

    HowToAcknowledge = bids_property_collapsed_character(
      name = "HowToAcknowledge", type = "optional", collapse = " "
    ),

    Funding = bids_property_character(name = "Funding", type = "optional", max_len = NULL),

    EthicsApprovals = bids_property_character(name = "EthicsApprovals", type = "optional", max_len = NULL),

    ReferencesAndLinks = bids_property_character(name = "ReferencesAndLinks", type = "optional", max_len = NULL),

    DatasetDOI = bids_property_character(name = "DatasetDOI", type = "optional"),

    parent_directory = bids_property_character(name = "parent_directory", type = "required", validator = validator_nonempty_string)

  ),

  validator = function(self) {

    if( !identical(self@DatasetType, "raw") ) {
      if(!length(self@GeneratedBy)) {
        bids_validator_warn("BIDS requires `GeneratedBy` to be provided for derivative dataset")
      }
    }
    NULL

  },
  hidden_names = "parent_directory"

)


## `format`
S7::method(format.generic, BIDSDatasetDescription) <- function(x, ..., indent = json_indent()) {
  li <- as.list(x, recursive = TRUE)
  li$GeneratedBy <- lapply(li$GeneratedBy, as.list)
  to_json(x = li, indent = indent)
}




bids_dataset_description_from_list <- function(x) {
  args <- drop_nulls(x)
  if(length(args$parent_directory) != 1 || is.na(args$parent_directory) || !nzchar(args$parent_directory) || args$parent_directory %in% c("/")) {
    stop("Please provide a path to the `parent_directory` that contains BIDS dataset_description.json")
  }
  re <- do.call(BIDSDatasetDescription, args)
  re
}



S7::method(get_bids_dataset_description, BIDSDatasetDescription) <- function(x, parent_directory, ...) {
  x
}

S7::method(get_bids_dataset_description, S7::class_missing) <- function(x, parent_directory, ...) {
  bids_dataset_description_from_list(list(..., parent_directory = parent_directory))
}

S7::method(get_bids_dataset_description, S7::class_list) <- function(x, parent_directory, ...) {
  # In case this is a fastmap2, also do not alter `x`
  x <- as.list(x)
  if(length(x$parent_directory) != 1 || is.na(x$parent_directory) || !nzchar(x$parent_directory)) {
    x$parent_directory <- parent_directory
  }
  bids_dataset_description_from_list(x)
}

S7::method(get_bids_dataset_description, S7::class_character) <- function(x, parent_directory, ...) {
  if(length(x) == 1 && !is.na(x) && endsWith(tolower(x), ".json") && file_exists(x)) {
    # json path
    source_path <- path_abs(x)
    parent_directory <- dirname(source_path)
    x <- from_json(file = source_path)
  } else {
    if(!startsWith(trimws(x), "{")) {
      sstr <- substr(x, 1, 20)
      if(nzchar(sstr)) {
        if(nchar(sstr) > 17) {
          sstr <- sprintf("\n\t%s...", sstr)
        } else {
          sstr <- sprintf("\n\t%s", sstr)
        }
      }
      stop("Unable to parse text: it is neither a path nor a JSON string.", sstr)
    }
    if(missing(parent_directory) || length(parent_directory) != 1 || is.na(parent_directory)) {
      parent_directory <- attr(x, "parent_directory")
      if(!length(parent_directory)) {
        source_path <- attr(x, "source_path")
        if(length(source_path) == 1) {
          parent_directory <- dirname(parent_directory)
        }
      }
    }
    x <- from_json(json_str = x)
  }
  x$parent_directory <- parent_directory
  bids_dataset_description_from_list(x)
}


# FIXME:
# TODO: Implement this
#
# path_to_nearest_dataset_description <- function(start, ...) {
#   path_to_nearest_file("dataset_description.json", start)
# }
#
# # link is as-is the items in dataset_description.json `DatasetLinks`
# # path the the enclosing directory
# handle_dataset_link <- function(link, directory) {
#   # like:
#   # list(`bids-root` = "", `freesurfer-root` = "derivatives/freesurfer")
#
#   # FIXME: support multiple protocols
#
#   S7::check_is_S7(directory, bids_directory_impl)
#   list(
#     link = link,
#     path = file_path(directory$abs_path, link)
#   )
# }
