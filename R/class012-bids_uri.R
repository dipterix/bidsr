# DIPSAUS DEBUG START
# uri <- "bids::sub-01/fmap:sub-01_dir-AP_epi.nii.gz"
# pattern <- "(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"

parse_bids_uri <- function(uri) {
  if(S7::S7_inherits(uri, BIDSURI)) {
    return(uri)
  }

  # https://bids-specification.readthedocs.io/en/stable/common-principles.html#uniform-resource-indicator

  # # https://bids-specification.readthedocs.io/en/stable/glossary.html#uri-formats
  # pattern <- "(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
  # if(!grepl(pattern, uri)) {
  #   stop("Not a valid BIDS URI")
  # }
  if(!startsWith(uri, "bids:")) {
    stop("BIDS URI must start with `bids:`")
  }

  str <- substr(uri, 6, nchar(uri))
  str <- strsplit(str, ":", fixed = TRUE)[[1]]
  dataset_name <- str[[1]]
  relative_path <- paste(str[-1], collapse = ":")
  list(dataset_name = dataset_name, relative_path = relative_path)
  # S7::new_object(S7::S7_object(), dataset_name = dataset_name, relative_path = relative_path)
}

#' 'BIDS' uniform resource indicator ('URI') class definition
#' @author Zhengjia Wang
#' @param uri 'URI' string or another 'BIDS-URI' object
#' @returns A \code{BIDSURI} instance.
#'
#' @examples
#'
#' # basic properties
#' uri <- BIDSURI("bids::sub-01/fmap/sub-01_dir-AP_epi.nii.gz")
#' uri
#' uri$relative_path
#' uri$dataset_name
#'
#' # set the entire uri
#' uri$format <- "bids:deriv1:sub-02/anat/sub-02_T1w.nii.gz"
#' uri
#'
#' # resolve BIDS URI (partial support)
#'
#'
#' # resolving a BIDS URI requires dataset_description.json
#' dataset_description <- get_bids_dataset_description(
#'   parent_directory = "/path/to/BIDS/folder",
#'   Name = "A dummy experiments",
#'   BIDSVersion = "1.6.0",
#'
#'   DatasetLinks = list(
#'     "deriv1" = "derivatives/derivative1",
#'     "phantoms" = "file:///data/phantoms"
#'   )
#' )
#'
#' uri <- BIDSURI("bids::sub-01/fmap/sub-01_dir-AP_epi.nii.gz")
#' resolved <- resolve_bids_path(uri, dataset_description)
#'
#' # resolved absolute path
#' print(resolved)
#'
#' # `raw_resolution` is relative to the parent directory where
#' # `dataset_description.json` is stored
#' attr(resolved, "raw_resolution")
#'
#' uri <- BIDSURI("bids:deriv1:sub-02/anat/sub-02_T1w.nii.gz")
#' resolved <- resolve_bids_path(uri, dataset_description)
#'
#' print(resolved)
#'
#' attr(resolved, "raw_resolution")
#'
#'
#' @export
BIDSURI <- new_bids_class(
  name = "BIDSURI",
  properties = list(
    dataset_name = bids_property_character(name = "dataset_name", type = "required"),
    relative_path = bids_property_character(
      name = "relative_path",
      type = "required",
      validator = function(value) {
        check <- validator_nonempty_string(value = value)
        if (!is.null(check)) {
          return(check)
        }
        value <- trimws(value)
        if (any(grepl("^[/\\\\]", value)) ) {
          return("BIDS URI must use relative path and cannot start with `/` nor `\\`.")
        }
        return()
      }
    ),
    format = bids_property_character(
      name = "format",
      type = "optional",
      setter = function(self, value) {
        parsed <- parse_bids_uri(value)
        self@dataset_name <- parsed$dataset_name
        self@relative_path <- parsed$relative_path
        self
      },
      getter = function(self) {
        sprintf("bids:%s:%s", self@dataset_name, self@relative_path)
      }
    )
  ),
  constructor = function(uri) {
    parsed <- parse_bids_uri(uri)
    S7::new_object(S7::S7_object(), dataset_name = parsed$dataset_name, relative_path = parsed$relative_path)
  }
)


S7::method(resolve_bids_path, BIDSURI) <- function(x, dataset_description, ...) {

  docstring <- "Usage -> bidsr::resolve_bids_path(x, dataset_description, ...)

  x: BIDS URI object; see `?BIDSURI`
  dataset_description: BIDS tabular instance of nearest
      `dataset_description.json`; see `?BIDSDatasetDescription`

Returns: a resolved absolute path.
  "
  use_docstring(dataset_description, docstring)


  # https://bids-specification.readthedocs.io/en/stable/common-principles.html#resolution-of-bids-uris
  # As of 1.10.0, BIDS URIs cannot be interpreted outside a BIDS dataset, as they require a dataset_description.json file to resolve.
  # a future version may specify an authority that would allow BIDS URIs to be resolved without reference to a local dataset_description.json.

  # I'll leave a future me to handle the cases where dataset_description.json is not required
  dataset_description <- get_bids_dataset_description(x = dataset_description, ...)

  if(nzchar(x@dataset_name)) {
    base_link <- dataset_description@DatasetLinks[[x@dataset_name]]
    if(length(base_link) != 1) {
      stop(
        "The given `dataset_description.json` does not contain dataset name: ", sQuote(x@dataset_name), ".\n",
        "  Available `DatasetLinks` keys are: ", paste(sQuote(names(dataset_description@DatasetLinks)), collapse = ", "), ".\n",
        "  based on: `", file_path(dataset_description@parent_directory, "dataset_description.json"),"`"
      )
    }

    raw_resolution <- file_path(base_link, x@relative_path)
  } else {
    raw_resolution <- x@relative_path
  }



  # Currently we only provide the naive resolution

  absolute_path <- path_abs(file_path(dataset_description@parent_directory, raw_resolution))


  structure(
    absolute_path,

    # We may want to support DOI, RAVE, ... but currently there is no common rules on how URI should be resolved
    # I guess this depends on different data archives?
    # Leaving raw_resolution for future
    raw_resolution = raw_resolution
  )
}
