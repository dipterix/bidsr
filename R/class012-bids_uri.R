# DIPSAUS DEBUG START
# uri <- "bids::sub-01/fmap:sub-01_dir-AP_epi.nii.gz"
# pattern <- "(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"

parse_bids_uri <- function(uri) {
  if(S7::S7_inherits(uri, bids_uri)) {
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
#' @param uri 'URI' string or another 'BIDS-URI' object
#' @returns A \code{bids_uri} instance.
#'
#' @examples
#'
#' # basic properties
#' uri <- bids_uri("bids::sub-01/fmap/sub-01_dir-AP_epi.nii.gz")
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
#' data_description <- dataset_description <- as_bids_dataset_description(
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
#' uri <- bids_uri("bids::sub-01/fmap/sub-01_dir-AP_epi.nii.gz")
#' resolved <- uri$resolve(data_description)
#'
#' # `raw_resolution` is relative to the parent directory where
#' # `dataset_description.json` is stored
#' resolved$raw_resolution
#' resolved$absolute_path
#'
#' uri <- bids_uri("bids:deriv1:sub-02/anat/sub-02_T1w.nii.gz")
#' resolved <- uri$resolve(data_description)
#' resolved$raw_resolution
#' resolved$absolute_path
#'
#'
#' @export
bids_uri <- new_bids_class(
  name = "bids_uri",
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
  methods = list(
    resolve = function(self, data_description, ...) {
      # https://bids-specification.readthedocs.io/en/stable/common-principles.html#resolution-of-bids-uris
      # As of 1.10.0, BIDS URIs cannot be interpreted outside a BIDS dataset, as they require a dataset_description.json file to resolve.
      # a future version may specify an authority that would allow BIDS URIs to be resolved without reference to a local dataset_description.json.

      # I'll leave a future me to handle the cases where dataset_description.json is not required
      data_description <- as_bids_dataset_description(x = data_description, ...)

      if(nzchar(self$dataset_name)) {
        base_link <- data_description$DatasetLinks[[self$dataset_name]]
        if(length(base_link) != 1) {
          stop("The given `dataset_description.json` does not contain dataset name: ", sQuote(self$dataset_name), ".\n  Available `DatasetLinks` keys are: ",
               paste(sQuote(names(data_description$DatasetLinks)), collapse = ", "), ".\n  based on: `", file_path(data_description$parent_directory, "dataset_description.json"), "`")
        }

        raw_resolution <- file_path(base_link, self$relative_path)
      } else {
        raw_resolution <- self$relative_path
      }



      # Currently we only provide the naive resolution

      list(
        # We may want to support DOI, RAVE, ... but currently there is no common rules on how URI should be resolved
        # I guess this depends on different data archives
        raw_resolution = raw_resolution,
        absolute_path = path_abs(file_path(data_description$parent_directory, raw_resolution))
      )
    }
  ),
  constructor = function(uri) {
    parsed <- parse_bids_uri(uri)
    S7::new_object(S7::S7_object(), dataset_name = parsed$dataset_name, relative_path = parsed$relative_path)
  }
)

