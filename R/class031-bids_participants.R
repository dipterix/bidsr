# TODO: Add `samples.tsv`, phenotype, ... from https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#phenotypic-and-assessment-data

preset_participants_meta <- local({
  meta <- NULL
  function(...) {
    if(is.null(meta)) {
      meta <<- bids_tabular_meta_sidecar(columns = list(
        participant_id = bids_tabular_column_descriptor(
          LongName    = "Participant ID",
          Description = "A participant identifier of the form sub-<label>, matching a participant entity found in the dataset",
          TermURL     = "https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#participants-file"
        ),
        species = bids_tabular_column_descriptor(
          LongName    = "Species",
          Description = "The species column SHOULD be a binomial species name from the NCBI Taxonomy (for example, homo sapiens, mus musculus, rattus norvegicus). For backwards compatibility, if species is absent, the participant is assumed to be homo sapiens.",
          TermURL     = "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi"
        ),
        age = bids_tabular_column_descriptor(
          LongName    = "Subject age",
          Description = "Numeric value in years (float or integer value). It is recommended to tag participant ages that are 89 or higher as 89+, for privacy purposes"
        ),
        sex = bids_tabular_column_descriptor(
          LongName    = "Sex",
          Description = 'String value indicating phenotypical sex, one of "male", "female", "other"',
          Levels = list(
            "male" = "male",
            "female" = "female",
            "other" = "other"
          )
        ),
        handedness = bids_tabular_column_descriptor(
          LongName    = "Subject handedness",
          Description = 'String value indicating one of "left", "right", "ambidextrous"',
          TermURL     = "https://bids-specification.readthedocs.io/en/stable/glossary.html#objects.columns.handedness",
          Levels = list(
            "left" = "left",
            "right" = "right",
            "ambidextrous" = "ambidextrous"
          )
        ),
        strain = bids_tabular_column_descriptor(
          LongName    = "Strain",
          Description = 'For species different from homo sapiens, string value indicating the strain of the species, for example: C57BL/6J'
        ),
        strain_rrid = bids_tabular_column_descriptor(
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
#' @description
#' A tabular containing a list of participants and their demographics.
#' The class is a child class of \code{\link{bids_tabular}}, hence see
#' the methods there.
#' The original specification is at
#' \url{https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#participants-file}.
#' @param content,meta see \code{\link{bids_tabular}}
#' @returns A \code{bids_tabular_participants} instance inheriting
#' \code{\link{bids_tabular}}.
#' @examples
#'
#'
#'
#' # basic
#' tabular <- bids_tabular_participants(
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
#'   # read tabular as bids_tabular_participants
#'   as_bids_tabular(file, cls = bids_tabular_participants)
#'
#'   # convert existing tabular
#'   tabular <- bids_tabular(
#'     data.frame(
#'       participant_id = "sub-001"
#'     )
#'   )
#'   tabular <- as_bids_tabular(tabular, cls = bids_tabular_participants)
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
bids_tabular_participants <- new_bids_tabular_class(
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
