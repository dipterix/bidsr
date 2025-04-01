# https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#sessions-file
#
# Sessions file
#
# Template:
#
#   sub-<label>/
#     sub-<label>_sessions.tsv
#
# Optional: Yes
#
# In case of multiple sessions there is an option of adding additional sessions.tsv files describing variables changing between sessions. In such case one file per participant SHOULD be added. These files MUST include a session_id column and describe each session by one and only one row. Column names in sessions.tsv files MUST be different from group level participant key column names in the participants.tsv file.
# Column name 	Requirement Level 	Data type 	Description
# session_id 	REQUIRED 	string 	A session identifier of the form ses-<label>, matching a session found in the dataset. There MUST be exactly one row for each session.
#
# Values in session_id MUST be unique.
#
# This column must appear first in the file.
# acq_time 	OPTIONAL 	string 	Acquisition time refers to when the first data point of the first run was acquired. Datetime format and their deidentification are described in Units.
#
# This column may appear anywhere in the file.
# pathology 	RECOMMENDED 	string or number 	String value describing the pathology of the sample or type of control. When different from healthy, pathology SHOULD be specified. The pathology may be specified in either samples.tsv or sessions.tsv, depending on whether the pathology changes over time.
#
# This column may appear anywhere in the file.
# Additional Columns 	OPTIONAL 	n/a 	Additional columns are allowed.
#
# _sessions.tsv example:
#
#   session_id  acq_time    systolic_blood_pressure
# ses-predrug 2009-06-15T13:45:30 120
# ses-postdrug    2009-06-16T13:45:30 100
# ses-followup    2009-06-17T13:45:30 110


preset_sessions_meta <- local({
  meta <- NULL
  function(...) {
    if(is.null(meta)) {
      meta <<- BIDSTabularMetaSidecar(columns = list(
        session_id = BIDSTabularColumnDescriptor(
          LongName    = "Session ID",
          Description = "[Required, string] A session identifier of the form ses-<label>, matching a session found in the dataset. There MUST be exactly one row for each session. Values in session_id MUST be unique. This column must appear first in the file."
        ),
        acq_time = BIDSTabularColumnDescriptor(
          LongName    = "Acquisition Time",
          Description = "[Optional, string] Acquisition time refers to when the first data point of the first run was acquired. Datetime format and their deidentification are described in Units. This column may appear anywhere in the file.",
          TermURL     = "https://bids-specification.readthedocs.io/en/stable/common-principles.html#units"
        ),
        pathology = BIDSTabularColumnDescriptor(
          LongName    = "Pathology",
          Description = "[Recommended, string or number] String value describing the pathology of the sample or type of control. When different from healthy, pathology SHOULD be specified. The pathology may be specified in either samples.tsv or sessions.tsv, depending on whether the pathology changes over time. This column may appear anywhere in the file."
        ),
        ...
      ))
    }
    return(meta)
  }
})

#' @title 'BIDS' sessions table class
#' @author Zhengjia Wang
#' @description
#' A tabular containing a list of sessions and their metadata.
#' The class is a child class of \code{\link{BIDSTabular}}, hence see
#' the methods there.
#' The original specification is at
#' \url{https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#sessions-file}.
#' @param content,meta see \code{\link{BIDSTabular}}
#' @returns A \code{BIDSTabularSessions} instance inheriting
#' \code{\link{BIDSTabular}}.
#' @examples
#'
#'
#'
#' # basic
#' tabular <- BIDSTabularSessions(data.frame(
#'   session_id = c("ses-predrug", "ses-postdrug", "ses-followup"),
#'   acq_time = c(
#'     "2009-06-15T13:45:30",
#'     "2009-06-16T13:45:30",
#'     "2009-06-17T13:45:30"
#'   ),
#'   systolic_blood_pressure = c(120, 100, 110)
#' ))
#' tabular
#'
#'
#' # convert existing tabular
#' tabular <- BIDSTabular(
#'   data.frame(
#'     acq_time = "2009-06-15T13:45:30",
#'     session_id = "ses-predrug",
#'     systolic_blood_pressure = 120
#'   )
#' )
#' tabular <- as_bids_tabular(tabular, cls = BIDSTabularSessions)
#' tabular
#'
#'
#' # save to tsv
#' tsv <- file.path(tempdir(), "sessions.tsv")
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
BIDSTabularSessions <- new_bids_tabular_class(
  table_name = "sessions",
  lower_case_column_names = TRUE,
  content_setter = function(self, value) {
    # table name already lower-cased, value is data.table

    # kindly reorder the table so the first column is "session_id"
    nms <- names(value)

    if(!identical(nms[[1]], "session_id")) {
      value <- value[, unique(c("session_id", nms)), with = FALSE]
    }

    session_ids <- value$session_id
    session_ids <- session_ids[!is.na(session_ids)]

    if(!all(grepl("^ses-", session_ids))) {
      stop("BIDS sessions.tsv requires that all `session_id` must start with 'ses-'")
    }


    if(any(duplicated(session_ids))) {
      warning("BIDS sessions.tsv requires that all `session_id` must be unique")
    }

    # if acq_time is present, convert
    value$acq_time <- bids_datetime_to_nanotime(value$acq_time)

    self@content <- value
    self
  },
  meta_preset = preset_sessions_meta()
)



