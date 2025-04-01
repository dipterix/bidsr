# TODO: Add `samples.tsv`, phenotype, ... from https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#phenotypic-and-assessment-data

preset_participants_meta <- local({
  meta <- NULL
  function(...) {
    if(is.null(meta)) {
      meta <<- BIDSTabularMetaSidecar(columns = list(
        participant_id = BIDSTabularColumnDescriptor(
          LongName    = "Participant ID",
          Description = "A participant identifier of the form sub-<label>, matching a participant entity found in the dataset",
          TermURL     = "https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#participants-file"
        ),
        species = BIDSTabularColumnDescriptor(
          LongName    = "Species",
          Description = "The species column SHOULD be a binomial species name from the NCBI Taxonomy (for example, homo sapiens, mus musculus, rattus norvegicus). For backwards compatibility, if species is absent, the participant is assumed to be homo sapiens.",
          TermURL     = "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi"
        ),
        age = BIDSTabularColumnDescriptor(
          LongName    = "Subject age",
          Description = "Numeric value in years (float or integer value). It is recommended to tag participant ages that are 89 or higher as 89+, for privacy purposes"
        ),
        sex = BIDSTabularColumnDescriptor(
          LongName    = "Sex",
          Description = 'String value indicating phenotypical sex, one of "male", "female", "other"',
          Levels = list(
            "male" = "male",
            "female" = "female",
            "other" = "other"
          )
        ),
        handedness = BIDSTabularColumnDescriptor(
          LongName    = "Subject handedness",
          Description = 'String value indicating one of "left", "right", "ambidextrous"',
          TermURL     = "https://bids-specification.readthedocs.io/en/stable/glossary.html#objects.columns.handedness",
          Levels = list(
            "left" = "left",
            "right" = "right",
            "ambidextrous" = "ambidextrous"
          )
        ),
        strain = BIDSTabularColumnDescriptor(
          LongName    = "Strain",
          Description = 'For species different from homo sapiens, string value indicating the strain of the species, for example: C57BL/6J'
        ),
        strain_rrid = BIDSTabularColumnDescriptor(
          LongName    = "Strain RRID",
          Description = 'For species different from homo sapiens, research resource identifier (RRID) of the strain of the species',
          TermURL     = "https://rrid.site/data/source/nlx_154697-1/search"
        ),
        ...
      ))
    }
    return(meta)
  }
})

#' @title 'BIDS' participant table class
#' @author Zhengjia Wang
#' @description
#' A tabular containing a list of participants and their demographics.
#' The class is a child class of \code{\link{BIDSTabular}}, hence see
#' the methods there.
#' The original specification is at
#' \url{https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#participants-file}.
#' @param content,meta see \code{\link{BIDSTabular}}
#' @param x R object such as file path, project instances, etc.
#' @param ... passed to other methods or ignored
#' @returns A \code{BIDSTabularParticipants} instance inheriting
#' \code{\link{BIDSTabular}}.
#' @examples
#'
#'
#'
#' # basic
#' tabular <- BIDSTabularParticipants(
#'   data.frame(
#'     participant_id = "sub-001"
#'   )
#' )
#' tabular
#'
#'
#' # Run `download_bids_examples()` first
#' examples <- download_bids_examples(test = TRUE)
#' if(!isFALSE(examples)) {
#'
#'   file <- file.path(examples, "ieeg_epilepsy_ecog", "participants.tsv")
#'
#'   # read tabular as BIDSTabularParticipants
#'   as_bids_tabular(file, cls = BIDSTabularParticipants)
#'
#'   # convert existing tabular
#'   tabular <- BIDSTabular(
#'     data.frame(
#'       participant_id = "sub-001"
#'     )
#'   )
#'   tabular <- as_bids_tabular(tabular, cls = BIDSTabularParticipants)
#'
#'   # save to tsv
#'   tsv <- file.path(tempdir(), "participants.tsv")
#'   paths <- save_bids_tabular(tabular, tsv)
#'   print(paths)
#'
#'   # use base R to read
#'   read.table(tsv, header = TRUE, na.strings = "n/a")
#'
#'   # get sidecar
#'   cat(readLines(paths$sidecar_path), sep = "\n")
#'
#'   unlink(tsv)
#'   unlink(paths$sidecar_path)
#' }
#'
#' @export
BIDSTabularParticipants <- new_bids_tabular_class(
  table_name = "participants",
  lower_case_column_names = TRUE,
  content_setter = function(self, value) {
    # table name already lower-cased, value is data.table

    # make sure participant_id is the first one
    nms <- names(value)
    if (!identical(nms[[1]], "participant_id")) {
      value <- value[, unique(c("participant_id", nms)), with = FALSE]
    }

    if (nrow(value)) {
      if (!all(startsWith(value$participant_id, "sub-"))) {
        warning(
          "BIDS participants.tsv requires that all `participant_id` must have format `sub-<label>`"
        )
      }

      if (!"species" %in% nms) {
        # The species column SHOULD be a binomial species name from the
        # NCBI Taxonomy (for example, homo sapiens, mus musculus, rattus
        # norvegicus). For backwards compatibility, if species is absent,
        # the participant is assumed to be homo sapiens.
        value$species <- "homo sapiens"
      }

      if (length(value$sex) > 0) {
        sex_lower <- tolower(value$sex)
        value$sex[sex_lower %in% c("m")] <- "male"
        value$sex[sex_lower %in% c("f")] <- "female"
        value$sex[sex_lower %in% c("o")] <- "other"
      }

      if (length(value$handedness) > 0) {
        handedness_lower <- tolower(value$handedness)
        value$handedness[handedness_lower %in% c("l")] <- "left"
        value$handedness[handedness_lower %in% c("r")] <- "right"
        value$handedness[handedness_lower %in% c("a")] <- "ambidextrous"
      }
    }

    self@content <- value
    self
  },
  meta_preset = preset_participants_meta(),
  prepare_save = NULL
)

## `get_bids_participants` generic
S7::method(get_bids_participants, S7::class_character) <- function(x, ...) {
  as_bids_tabular(x, cls = BIDSTabularParticipants)
}

S7::method(get_bids_participants, BIDSTabularParticipants) <- function(x, ...) {
  x
}


S7::method(get_bids_participants, BIDSTabular) <- function(x, ...) {
  as_bids_tabular(x = x, cls = BIDSTabularParticipants)
}











