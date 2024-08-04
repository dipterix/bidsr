# Each BIDS dataset is treated as a RAVE project

# bids_project_path <-
#   system.file("examples", "ieeg_epilepsy_ecog", package = "bidsr")
#
# project <- bids_project(bids_project_path)

bids_directory <- S7::new_class(
  name = "bids_directory",
  package = "bidsr",
  abstract = FALSE,
  parent = bids_object,
  properties = list(

  )
)

bids_project_impl <- S7::new_class(
  name = "bids_project",
  package = "bidsr",
  abstract = FALSE,
  parent = bids_object,
  properties = list(
    base_path = property_immutable_string("base_path"),
    sourcedata_path = property_immutable_string("sourcedata_path"),
    rawdata_path = property_immutable_string("rawdata_path"),
    derivatives_path = property_immutable_string("derivatives_path"),
    phenotype_path = property_immutable_string("phenotype_path"),
    stimuli_path = property_immutable_string("stimuli_path"),
    # dataset_description, meta, ...
    info = S7::new_property(
      class = bids_map,
      validator = function(value) {
        dataset_description <- value@impl$get("dataset_description")
        participants <- value@impl$get("participants")
        if(!S7::S7_inherits(dataset_description,
                            bids_dataset_description_impl)) {
          return("BIDS project requires a top-level `dataset_description.json`")
        }
        if(!S7::S7_inherits(participants,
                            bids_participants_impl)) {
          return("Missing participants informaition.")
        }
        return()
      },
      setter = function(self, value) {
        value$project <- self
        self@info <- value
        self
      }
    ),
    dataset_description = S7::new_property(
      class = bids_dataset_description_impl,
      getter = function(self) {
        self@info$dataset_description
      },
      setter = function(self, value) {
        self@info$dataset_description <- value
        self
      }
    ),
    participants = S7::new_property(
      class = bids_participants_impl,
      getter = function(self) {
        self@info$participants
      },
      setter = function(self, value) {
        self@info$participants <- value
        self
      }
    ),
    format = property_format(function(self) {
      paste(
        sep = "\n",
        c(
          sprintf("<BIDS Project> @ %s", self@base_path),
          sprintf("  Name        : %s", self@dataset_description@Name),
          sprintf("  BIDSVersion : %s", self@dataset_description@BIDSVersion),
          "",
          "Paths (relative to the base path)",
          sprintf("  rawdata     : %s/", self@rawdata_path),
          sprintf("  sourcedata  : %s/", self@sourcedata_path),
          sprintf("  derivatives : %s/", self@derivatives_path),
          sprintf("  phenotype   : %s/", self@phenotype_path),
          sprintf("  stimuli     : %s/", self@stimuli_path),
          "",
          format(self@participants, indent = 0)
        )
      )

    }),
    print = property_print_format
  ),
  constructor = function(info, base_path, sourcedata_path, rawdata_path,
                         derivatives_path, phenotype_path, stimuli_path) {
    self <- S7::new_object(
      S7::S7_object(),
      info = info,
      base_path = base_path,
      sourcedata_path = sourcedata_path,
      rawdata_path = rawdata_path,
      derivatives_path = derivatives_path,
      phenotype_path = phenotype_path,
      stimuli_path = stimuli_path
    )

    info@impl$set("project", self)
    info@impl$set("directory", self)
    self
  }
)

#' @export
bids_project <- function(
    base_path, rawdata = "", sourcedata = "sourcedata",
    derivatives = "derivatives", phenotype = "phenotype",
    stimuli = "stimuli", new = FALSE
) {

  rawdata_path <- file_path(base_path, rawdata)
  dset_descr_path <- file_path(rawdata_path, "dataset_description.json")
  if(!file_exists(dset_descr_path)) {
    if(!new) {
      stop("Path is not a valid BIDS project folder:\n  ", base_path)
    }
    if(!dir_exists(base_path)) { dir_create(base_path) }
    dataset_description <- bids_dataset_description(
      method = "list",
      Name = "Unnamed",
      DatasetType = "raw",
      GeneratedBy = list(DEFAULT_GENERATED_BY()),
      BIDSVersion = BIDS_VERSION
    )
    writeLines(format(dataset_description), con = dset_descr_path)
  } else {
    dataset_description <- bids_dataset_description(
      dset_descr_path,
      method = "json_path"
    )
  }


  base_path <- path_abs(base_path)

  # get all subject table
  subjects <- list.files(
    rawdata_path,
    pattern = "^sub-",
    include.dirs = TRUE,
    no.. = TRUE,
    ignore.case = TRUE,
    full.names = FALSE,
    recursive = FALSE,
    all.files = FALSE
  )

  # get participants table
  participants_path <- file_path(rawdata_path, "participants.tsv")

  if(file_exists(participants_path)) {
    participants <- bids_participants(participants_path)
    missing_subjects <- subjects[!subjects %in% participants@data$participant_id]
    n_missing <- length(missing_subjects)
    if(n_missing) {
      bids_validator_warn(sprintf(
        "Missing subject [%s] from the `participants.tsv`",
        paste(missing_subjects, collapse = ", ")
      ))
      headers <- names(participants@header@columns)
      missing_table <- as.data.frame(structure(
        names = headers,
        lapply(headers, function(nm) {
          if(identical(nm, "participant_id")) {
            return(missing_subjects)
          }
          return(rep(NA, n_missing))
        })
      ))
      participants@data <- rbind(participants@data, missing_table)
    }
  } else {
    participants <- bids_participants(data.frame(participant_id = subjects))
    bids_write_tabular(participants, participants_path)
  }

  info <- bids_map(parent = NULL, search_depth = 0L)
  info@impl$mset(
    dataset_description = dataset_description,
    participants = participants
  )

  rawdata_path <- path_rel(path_abs(rawdata_path), base_path)
  sourcedata_path <- sourcedata
  derivatives_path <- derivatives
  phenotype_path <- phenotype
  stimuli_path <- stimuli

  re <-
    bids_project_impl(
      info = info,
      base_path = base_path,
      sourcedata_path = sourcedata_path,
      rawdata_path = rawdata_path,
      derivatives_path = derivatives_path,
      phenotype_path = phenotype_path,
      stimuli_path = stimuli_path
    )
  re

}
