# Specification: https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#samples-file

preset_samples_meta <- local({
  meta <- NULL
  function(...) {
    if(is.null(meta)) {
      meta <<- BIDSTabularMetaSidecar(columns = list(
        sample_id = BIDSTabularColumnDescriptor(
          LongName    = "Sample ID",
          Description = "[Required, string] A sample identifier of the form `sample-<label>`, matching a sample entity found in the dataset",
          TermURL     = "https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#samples-file"
        ),
        participant_id = BIDSTabularColumnDescriptor(
          LongName    = "Participant ID",
          Description = "[Required, string] A participant identifier of the form `sub-<label>`, matching a participant entity found in the dataset",
          TermURL     = "https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#participants-file"
        ),
        sample_type = BIDSTabularColumnDescriptor(
          LongName    = "Sample Type",
          Description = "[Required, string] Biosample type defined by ENCODE Biosample Type",
          TermURL     = "https://www.encodeproject.org/profiles/biosample_type/",
          Levels = list(
            "cell line" = "cell line",
            "in vitro differentiated cells" = "in vitro differentiated cells",
            "primary cell" = "primary cell",
            "cell-free sample" = "cell-free sample",
            "cloning host" = "cloning host",
            "tissue" = "tissue",
            "whole organisms" = "whole organisms",
            "organoid" = "organoid",
            "technical sample" = "technical sample"
          )
        ),
        pathology = BIDSTabularColumnDescriptor(
          LongName    = "Pathology",
          Description = "[Recommended, string] String value describing the pathology of the sample or type of control. When different from healthy, pathology SHOULD be specified. The pathology may be specified in either `samples.tsv` or `sessions.tsv`, depending on whether the pathology changes over time."
        ),
        derived_from = BIDSTabularColumnDescriptor(
          LongName    = "Derived From",
          Description = "[Recommended, string] `sample-<label>` entity from which a sample is derived, for example a slice of tissue (`sample-02`) derived from a block of tissue (`sample-01`)."
        ),
        ...
      ))
    }
    return(meta)
  }
})


#' @title 'BIDS' samples table class
#' @description
#' A tabular containing a list of samples and their metadata.
#' The class is a child class of \code{\link{BIDSTabular}}, hence see
#' the methods there.
#' The original specification is at
#' \url{https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#samples-file}.
#' @param content,meta see \code{\link{BIDSTabular}}
#' @param x R object such as file path, project instances, etc.
#' @param ... passed to other methods or ignored
#' @returns A \code{BIDSTabularSamples} instance inheriting
#' \code{\link{BIDSTabular}}.
#' @examples
#'
#'
#' # basic
#' tabular <- BIDSTabularSamples(
#'   data.frame(
#'     sample_id = "sample-001",
#'     participant_id = "sub-001",
#'     sample_type = "cell line"
#'   )
#' )
#' tabular
#'
#'
#' # convert existing tabular
#' tabular <- BIDSTabular(
#'   data.frame(
#'     sample_id = "sample-001",
#'     participant_id = "sub-001",
#'     sample_type = "cell line"
#'   )
#' )
#' tabular <- as_bids_tabular(tabular, cls = BIDSTabularSamples)
#'
#'
#' # save to tsv
#' tsv <- file.path(tempdir(), "samples.tsv")
#' paths <- save_bids_tabular(tabular, tsv)
#' print(paths)
#'
#' # use base R to read
#' read.table(tsv, header = TRUE, na.strings = "n/a")
#'
#' # get sidecar
#' cat(readLines(paths$sidecar_path), sep = "\n")
#'
#' # clean up
#' unlink(tsv)
#' unlink(paths$sidecar_path)
#'
#'
#' @export
BIDSTabularSamples <- new_bids_tabular_class(
  table_name = "samples",
  lower_case_column_names = TRUE,
  content_setter = function(self, value) {
    # table name already lower-cased, value is data.table

    # kindly reorder the table so the first 3 columns are "sample_id", "participant_id", "sample_type"
    nms <- names(value)
    value <- value[, unique(c("sample_id", "participant_id", "sample_type", nms)), with = FALSE]

    # if(nrow(value)) {
    #   value$sample_type <- tolower(value$sample_type)
    # }

    self@content <- value
    self
  },
  meta_preset = preset_samples_meta(),
  prepare_save = NULL
)


## `get_bids_samples` generic
S7::method(get_bids_samples, S7::class_character) <- function(x, ...) {
  as_bids_tabular(x, cls = BIDSTabularSamples)
}

S7::method(get_bids_samples, BIDSTabularSamples) <- function(x, ...) {
  x
}

S7::method(get_bids_samples, BIDSTabular) <- function(x, ...) {
  as_bids_tabular(x = x, cls = BIDSTabularSamples)
}
