#' @export
bids_uri <- S7::new_class(
  name = "bids_uri",
  parent = bids_object,
  package = "bidsr",
  properties = list(
    dataset_name = bids_required(class = S7::class_character),
    rel_path = bids_required(
      class = S7::class_character,
      validator = function(value) {
        value <- trimws(value)
        if(startsWith(value, "/")) {
          return("BIDS URI must use relative path.")
        }
        return()
      },
      setter = function(self, value) {
        self@rel_path <- paste(strsplit(value, "[/|\\\\]+")[[1]], collapse = "/")
        self
      }
    ),
    format = property_format(function(self) {
        sprintf("bids:%s:%s", self@dataset_name, self@rel_path)
      }
    )
  ),
  constructor = function(uri) {
    if(!startsWith(uri, "bids:")) {
      stop("BIDS URI must start with `bids:`")
    }
    # uri <- "bids::sub-01/fmap:sub-01_dir-AP_epi.nii.gz"
    str <- substr(uri, 6, nchar(uri))
    str <- strsplit(str, ":", fixed = TRUE)[[1]]
    dataset_name <- str[[1]]
    rel_path <- paste(str[-1], collapse = ":")
    S7::new_object(S7::S7_object(), dataset_name = dataset_name, rel_path = rel_path)
  }
)

#' @returns Path relative to the nearest \code{'dataset_description.json'}
#' @export
resolve_bids_uri <- function(uri, source = NULL) {
  # source can be a path containing data_description.json
  # or a bids_dataset_description object

  if(!S7::S7_inherits(uri, bids_uri)) {
    uri <- bids_uri(uri)
  }

  path <- NA
  if(!is.null(source)) {
    if(!S7::S7_inherits(source, bids_dataset_description_impl)) {
      if(is.character(source)) {
        path <- path_to_nearest_file("dataset_description.json", source)
        if(is.na(path)) {
          source <- bids_dataset_description(source, method = "json_string")
        } else {
          source <- bids_dataset_description(path, method = "json_path")
        }
      } else if (is.list(source)) {
        source <- bids_dataset_description(.list = source, method = "default")
      }
    }
    if(is.na(path)) {
      path <- attr(source, ".bids_source_path")
    }
    dset_lists <- source@DatasetLinks
  } else {
    dset_lists <- list()
  }

  re <- NA
  dset_name <- uri@dataset_name

  at <- list(
    description_path = path,
    link_path = NA
  )
  if(!nzchar(dset_name)) {
    # Relative to the nearest data_description.json
    re <- path_norm(uri@rel_path)
    if(!is.na(path)) {
      at$link_path <- dirname(path)
    }
  } else {
    linked_path <- dset_lists[[dset_name]]
    if(length(linked_path) == 0) {
      stop("Cannot find dataset links for the dataset name `", dset_name, "`")
    }
    at$link_path <- linked_path
    re <- file_path(linked_path[[1]], uri@rel_path)
  }

  attr(re, "description_path") <- at$description_path
  attr(re, "link_path") <- at$link_path

  re

}
