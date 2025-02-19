# Specification: https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#samples-file

preset_samples_meta <- local({
  meta <- NULL
  function(...) {
    if(is.null(meta)) {
      meta <<- bids_tabular_meta_sidecar(columns = list(
        sample_id = bids_tabular_column_descriptor(
          LongName    = "Sample ID",
          Description = "[Required, string] A sample identifier of the form `sample-<label>`, matching a sample entity found in the dataset",
          TermURL     = "https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#samples-file"
        ),
        participant_id = bids_tabular_column_descriptor(
          LongName    = "Participant ID",
          Description = "[Required, string] A participant identifier of the form `sub-<label>`, matching a participant entity found in the dataset",
          TermURL     = "https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#participants-file"
        ),
        sample_type = bids_tabular_column_descriptor(
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
        pathology = bids_tabular_column_descriptor(
          LongName    = "Pathology",
          Description = "[Recommended, string] String value describing the pathology of the sample or type of control. When different from healthy, pathology SHOULD be specified. The pathology may be specified in either `samples.tsv` or `sessions.tsv`, depending on whether the pathology changes over time."
        ),
        derived_from = bids_tabular_column_descriptor(
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
#' The class is a child class of \code{\link{bids_tabular}}, hence see
#' the methods there.
#' The original specification is at
#' \url{https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#samples-file}.
#' @param content,meta see \code{\link{bids_tabular}}
#' @returns A \code{bids_tabular_samples} instance inheriting
#' \code{\link{bids_tabular}}.
#' @examples
#'
#'
#' # basic
#' tabular <- bids_tabular_samples(
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
#' tabular <- bids_tabular(
#'   data.frame(
#'     sample_id = "sample-001",
#'     participant_id = "sub-001",
#'     sample_type = "cell line"
#'   )
#' )
#' tabular <- as_bids_tabular(tabular, cls = bids_tabular_samples)
#'
#'
#' # save to tsv
#' tsv <- file.path(tempdir(), "samples.tsv")
#' paths <- tabular$save(tsv)
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
bids_tabular_samples <- new_bids_tabular_class(
  table_name = "samples",
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

