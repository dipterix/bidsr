# Specification: https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#scans-file
#
# Template:
#
# sub-<label>/
#   [ses-<label>/]
#     sub-<label>[_ses-<label>]_scans.tsv
#     sub-<label>[_ses-<label>]_scans.json
#
# Optional: Yes
#
# The purpose of this file is to describe timing and other properties of each recording file within one session. In general, each of these files SHOULD be described by exactly one row.
#
# For file formats that are based on several files of different extensions, or a directory of files with different extensions (multi-file file formats), only that file SHOULD be listed that would also be passed to analysis software for reading the data. For example for BrainVision data (.vhdr, .vmrk, .eeg), only the .vhdr SHOULD be listed; for EEGLAB data (.set, .fdt), only the .set file SHOULD be listed; and for CTF data (.ds), the whole .ds directory SHOULD be listed, and not the individual files in that directory.
#
# Some neural recordings consist of multiple parts, that span several files, but that share the same extension. For example in entity-linked file collections, commonly used for qMRI, where recordings may be linked through entities such as echo and part (using .nii or .nii.gz extensions). For another example consider the case of large files in .fif format that are linked through the split entity (see Split files). Such recordings MUST be documented with one row per file (unlike the case of multi-file file formats described above).
# Column name 	Requirement Level 	Data type 	Description
# filename 	REQUIRED 	string 	Relative paths to files. There MUST be exactly one row for each file.
#
# Values in filename MUST be unique.
#
# This column must appear first in the file.
# acq_time 	OPTIONAL 	string 	Acquisition time refers to when the first data point in each run was acquired. Furthermore, if this header is provided, the acquisition times of all files from the same recording MUST be identical. Datetime format and their deidentification are described in Units.
#
# This column may appear anywhere in the file.
# Additional Columns 	OPTIONAL 	n/a 	Additional columns are allowed.
#
# Additional fields can include external behavioral measures relevant to the scan. For example vigilance questionnaire score administered after a resting state scan. All such included additional fields SHOULD be documented in an accompanying _scans.json file that describes these fields in detail (see Tabular files).
#
# Example _scans.tsv:
#
#   filename    acq_time
# func/sub-control01_task-nback_bold.nii.gz   1877-06-15T13:45:30
# func/sub-control01_task-motor_bold.nii.gz   1877-06-15T13:55:33
# meg/sub-control01_task-rest_split-01_meg.nii.gz 1877-06-15T12:15:27
# meg/sub-control01_task-rest_split-02_meg.nii.gz 1877-06-15T12:15:27


preset_scans_meta <- local({
  meta <- NULL
  function(...) {
    if(is.null(meta)) {
      meta <<- BIDSTabularMetaSidecar(columns = list(
        filename = BIDSTabularColumnDescriptor(
          LongName    = "Filename",
          Description = "[Required, string] Relative paths to files. There MUST be exactly one row for each file. Values in filename MUST be unique. This column must appear first in the file."
        ),
        acq_time = BIDSTabularColumnDescriptor(
          LongName    = "Acquisition Time",
          Description = "[Optional, string] Acquisition time refers to when the first data point in each run was acquired. Furthermore, if this header is provided, the acquisition times of all files from the same recording MUST be identical. Datetime format and their deidentification are described in Units. This column may appear anywhere in the file.",
          TermURL     = "https://bids-specification.readthedocs.io/en/stable/common-principles.html#units"
        ),
        ...
      ))
    }
    return(meta)
  }
})


#' @title 'BIDS' scans table class
#' @author Zhengjia Wang
#' @description
#' A tabular containing a list of scans and their metadata.
#' The class is a child class of \code{\link{BIDSTabular}}, hence see
#' the methods there.
#' The original specification is at
#' \url{https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#scans-file}.
#' @param content,meta see \code{\link{BIDSTabular}}
#' @returns A \code{BIDSTabularScans} instance inheriting
#' \code{\link{BIDSTabular}}.
#' @examples
#'
#'
#' # basic
#' tabular <- BIDSTabularScans(
#'   data.frame(
#'     filename = c(
#'       "func/sub-control01_task-nback_bold.nii.gz",
#'       "func/sub-control01_task-motor_bold.nii.gz",
#'       "meg/sub-control01_task-rest_split-01_meg.nii.gz"
#'     ),
#'     acq_time = c(
#'       "1877-06-15T13:45:30",
#'       "1877-06-15T13:55:33",
#'       "1877-06-15T12:15:27"
#'     )
#'   )
#' )
#'
#' # No ending Z, time is interpreted as local time
#' # tabular uses UTC time
#' tabular
#'
#'
#' # convert existing tabular
#' tabular <- BIDSTabular(
#'   data.frame(
#'     filename = "func/sub-control01_task-nback_bold.nii.gz",
#'     acq_time = "1877-06-15T13:45:30"
#'   )
#' )
#' tabular <- as_bids_tabular(tabular, cls = BIDSTabularScans)
#' tabular
#'
#' # save to tsv
#' tsv <- file.path(tempdir(), "scans.tsv")
#' paths <- save_bids_tabular(tabular, tsv)
#'
#' print(paths)
#'
#'
#' # use base R to read
#' read.table(tsv, header = TRUE, na.strings = "n/a")
#'
#' # get sidecar
#' cat(readLines(paths$sidecar_path), sep = "\n")
#'
#'
#' # clean up
#' unlink(tsv)
#' unlink(paths$sidecar_path)
#'
#'
#'
#' @export
BIDSTabularScans <- new_bids_tabular_class(
  table_name = "scans",
  lower_case_column_names = TRUE,
  content_setter = function(self, value) {
    # table name already lower-cased, value is data.table

    # kindly reorder the table so the first column is "filename"
    nms <- names(value)

    if(!identical(nms[[1]], "filename")) {
      value <- value[, unique(c("filename", nms)), with = FALSE]
    }

    filenames <- value$filename
    filenames <- filenames[!is.na(filenames)]
    if(any(duplicated(filenames))) {
      warning("BIDS scans.tsv requires that all `filename` must be unique")
    }

    # if acq_time is present, convert
    value$acq_time <- bids_datetime_to_nanotime(value$acq_time)

    self@content <- value
    self
  },
  meta_preset = preset_scans_meta()
)


