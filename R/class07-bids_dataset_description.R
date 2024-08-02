# https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html
# Modality agnostic files

# DIPSAUS DEBUG START
# dataset_description <- bids_dataset_description(
#   Name = "The mother of all experiments",
#   BIDSVersion = "1.6.0",
#   #DatasetType = "raw",
#   License = "CC0",
#   Authors = c("Paul Broca", "Carl Wernicke"),
#   Acknowledgements = paste(
#     "Special thanks to Korbinian Brodmann for help in formatting",
#     "this dataset in BIDS. We thank Alan Lloyd Hodgkin and Andrew",
#     "Huxley for helpful comments and discussions about the",
#     "experiment and manuscript; Hermann Ludwig Helmholtz for",
#     "administrative support; and Claudius Galenus for providing",
#     "data for the medial-to-lateral index analysis."
#   ),
#   HowToAcknowledge = paste(
#     "Please cite this paper:",
#     "https://www.ncbi.nlm.nih.gov/pubmed/001012092119281"
#   ),
#   Funding = c(
#     "National Institute of Neuroscience Grant F378236MFH1",
#     "National Institute of Neuroscience Grant 5RMZ0023106"
#   ),
#   EthicsApprovals = paste(
#     "Army Human Research Protections Office",
#     "(Protocol ARL-20098-10051, ARL 12-040, and ARL 12-041)"
#   ),
#   ReferencesAndLinks = c(
#     "https://www.ncbi.nlm.nih.gov/pubmed/001012092119281",
#     paste(
#       "Alzheimer A., & Kraepelin, E. (2015).",
#       "Neural correlates of presenile dementia in humans.",
#       "Journal of Neuroscientific Data, 2, 234001.",
#       "doi:1920.8/jndata.2015.7"
#     )
#   ),
#   DatasetDOI = "doi:10.0.2.3/dfjj.10",
#   HEDVersion = "8.0.0",
#   GeneratedBy = list(
#     list(
#       Name = "reproin",
#       Version = "0.6.0",
#       Container = list(
#         Type = "docker",
#         Tag = "repronim/reproin:0.6.0"
#       )
#     )
#   ),
#   SourceDatasets = list(
#     list(
#       URL = "s3://dicoms/studies/correlates",
#       Version = "April 11 2011"
#     )
#   )
# ) -> x

# str <- rjson::toJSON(as.list(x))
#
# bids_dataset_description(str, method = "json_string")
# x <- bids_generated_by(
#   Name = "reproin",
#   Version = "0.6.0",
#   Container = list(
#     Type = "docker",
#     Tag = "repronim/reproin:0.6.0"
#   )
# )

bids_generated_by <- S7::new_class(
  name = "bids_generated_by",
  parent = bids_object,
  abstract = FALSE,
  package = "bidsr",
  properties = list(
    Name = bids_required(
      class = S7::class_character,
      validator = validator_nonempty_string
    ),
    Version = bids_optional(class = S7::class_character),
    Description = bids_optional(class = S7::class_character),
    CodeURL = bids_optional(class = S7::class_character),
    Container = property_named_list,

    # --- active bindings ---
    format = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        rjson::toJSON(as.list(self, recursive = TRUE), indent = json_indent())
      }
    )
  )
)

bids_dataset_description_impl <- S7::new_class(
  name = "bids_dataset_description",
  parent = bids_object,
  abstract = FALSE,
  package = "bidsr",
  properties = list(
    # --- Required ---
    Name = bids_required(
      class = S7::class_character,
      validator = validator_nonempty_string
    ),
    BIDSVersion = bids_required(
      class = S7::class_character,
      validator = validator_nonempty_string
    ),
    # --- Conditionally required ---
    # FIXME: REQUIRED if BIDS URIs are used.
    # It's also object of strings (what's that???)
    DatasetLinks = property_named_list,

    # --- Recommended ---
    # array of strings
    HEDVersion = S7::new_property(class = S7::class_character),

    DatasetType = bids_optional(
      class = S7::class_character,
      getter = function(self) {
        # FIXME when S7 bug is fixed
        v <- get_prop_in_getter(self, "DatasetType")
        if(length(v) != 1) {
          v <- "raw"
        }
        v
      },
      validator = function(value) {
        if(length(value) != 1) { return() }
        if( value %in% c("raw", "derivative") ) { return() }
        return("DatasetType must be raw or derivative")
      }
    ),

    License = bids_optional(class = S7::class_character),

    Authors = S7::new_property(class = S7::class_character),

    # array of objects
    GeneratedBy = S7::new_property(
      class = S7::class_list,
      validator = validator_unnamed_list,
      setter = function(self, value) {
        self@GeneratedBy <- structure(
          names = NULL,
          lapply(value, function(x) {
            S7::check_is_S7(x, class = bids_generated_by)
            x
          })
        )
        self
      }
    ),
    SourceDatasets = S7::new_property(
      class = S7::class_list,
      validator = validator_unnamed_list,
      setter = function(self, value) {
        self@SourceDatasets <- unname(value)
        self
      }
    ),

    # --- Optional ---
    # Might be too long, using make_collapsed_string_property
    Acknowledgements = make_collapsed_string_property(
      name = "Acknowledgements", requirement = "optional"
    ),

    HowToAcknowledge = make_collapsed_string_property(
      name = "HowToAcknowledge", requirement = "optional"
    ),

    Funding = S7::new_property(class = S7::class_character),

    EthicsApprovals = S7::new_property(class = S7::class_character),

    ReferencesAndLinks = S7::new_property(class = S7::class_character),

    DatasetDOI = bids_optional(class = S7::class_character),

    # --- active bindings ---
    format = S7::new_property(
      class = S7::class_character,
      getter = function(self) {
        li <- as.list(self, recursive = TRUE)
        li$GeneratedBy <- lapply(li$GeneratedBy, as.list)
        rjson::toJSON(li, indent = json_indent())
      }
    )

  ),

  validator = function(self) {

    if( !identical(self@DatasetType, "raw") ) {
      if(!length(self@GeneratedBy)) {
        bids_validator_warn("BIDS requires `GeneratedBy` to be provided for derivative dataset")
      }
    }
    NULL

  }

)



#' @export
bids_dataset_description <- function(
    ..., method = c("default", "json_path", "json_string"),
    .list = NULL) {
  method <- match.arg(method)
  if(method == "default") {
    dat <- c(list(...), .list)
  } else {
    str <- c(...)
    if( method == "json_path" ) {
      json_path <- str[[1]]
      stopifnot(file.exists(json_path))
      json <- readLines(json_path)
    } else {
      json <- str
    }
    dat <- rjson::fromJSON(paste(json, collapse = "\n"), simplify = TRUE)
  }

  dat <- drop_nulls(dat)

  generated_by <- dat$GeneratedBy
  if(length(generated_by)) {
    dat$GeneratedBy <- drop_nulls(
      lapply(generated_by, function(kargs) {
        if(length(kargs)) {
          do.call(bids_generated_by, kargs)
        } else {
          NULL
        }
      })
    )
  }

  do.call(bids_dataset_description_impl, dat)
}


