# TODO: Add `samples.tsv`, phenotype, ... from https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#phenotypic-and-assessment-data


DEFAULT_PARTICIPANT_DESCRIPTORS <- bids_tabular_header(
  columns = list(
    age = bids_tabular_colname(
      name = "age",
      descriptors = list(
        Description = "age of the participant"
      )
    ),
    sex = bids_tabular_colname(
      name = "sex",
      descriptors = list(
        Description = "sex of the participant as reported by the participant",
        Levels = list(
          "male" = "male",
          "female" = "female",
          "other" = "other"
        )
      )
    ),
    handedness = bids_tabular_colname(
      name = "handedness",
      descriptors = list(
        Description = "handedness of the participant as reported by the participant",
        Levels = list(
          "left" = "left",
          "right" = "right",
          "ambidextrous" = "ambidextrous"
        )
      )
    )
  )
)

bids_participants_impl <- S7::new_class(
  name = "bids_participants",
  parent = bids_tabular_data,
  abstract = FALSE,
  package = "bidsr",
  properties = list(
    data = S7::new_property(
      S7::class_data.frame,
      validator = function(value) {
        nms <- names(value)
        if(!"participant_id" %in% nms) {
          return("BIDS participant table must contain column `participant_id`")
        }
        if(nrow(value) && !all(startsWith(value$participant_id, "sub-"))) {
          return("BIDS participant ID must have format `sub-<label>`")
        }
        return()
      },
      setter = function(self, value) {
        if(!is.character(value$participant_id)) {
          value$participant_id <- as.character(value$participant_id)
        }
        nr <- nrow(value)
        if(nr) {
          nms <- names(value)
          if("species" %in% nms && !is.character(value$species)) {
            value$species <- as.character(value$species)
          }
          if("age" %in% nms && !is.numeric(value$age)) {
            value$age <- as.numeric(value$age)
          }
          if("sex" %in% nms) {
            sex <- rep("other", nr)
            sex_ <- tolower(value$sex)
            sex[startsWith(sex_, "m")] <- "male"
            sex[startsWith(sex_, "f")] <- "female"
            value$sex <- sex
          }
          if("handedness" %in% nms) {
            handedness <- rep("ambidextrous", nr)
            handedness_ <- tolower(value$handedness)
            handedness[startsWith(handedness_, "l")] <- "left"
            handedness[startsWith(handedness_, "r")] <- "right"
            value$handedness <- handedness
          }
          if("strain" %in% nms && !is.character(value$strain)) {
            value$strain <- as.character(value$strain)
          }
          if("strain_rrid" %in% nms && !is.character(value$strain_rrid)) {
            value$strain_rrid <- as.character(value$strain_rrid)
          }
        }
        self@data <- value
        self
      }
    )
  )
)

#' @export
bids_participants <- function(x, header = NULL) {
  # DIPSAUS DEBUG START
  # x <- data.frame(
  #   participant_id = "sub-01",
  #   age = 66,
  #   species = "homo sapiens"
  # )
  new_bids_builtin_tabular(
    x = x,
    header = header,
    as_class = bids_participants_impl,
    default_descriptors = DEFAULT_PARTICIPANT_DESCRIPTORS@columns
  )

  # if( S7::S7_inherits(x, bids_participants_impl) ) {
  #   impl <- x
  # } else if ( S7::S7_inherits(x, bids_tabular_data) ) {
  #   impl <- bids_participants_impl(header = x@header, data = x@data)
  # } else if( is.character(x) ) {
  #   # x is path to the file
  #   impl <- bids_read_tabular(x, as_class = bids_participants_impl)
  # } else {
  #   impl <- bids_participants_impl(data = x)
  # }
  #
  # header_names <- names(impl@data)
  #
  # if(length(header)) {
  #   S7::check_is_S7(header, bids_tabular_header)
  # } else {
  #   header <- bids_tabular_header()
  # }
  #
  # header@columns <- drop_nulls(lapply(header_names, function(nm) {
  #   descr <- header@columns[[nm]]
  #   if (!is.null(descr)) {
  #     return(descr)
  #   }
  #   descr <- DEFAULT_PARTICIPANT_DESCRIPTORS@columns[[nm]]
  #   if (!is.null(descr)) {
  #     return(descr)
  #   }
  #   if(nm == "participant_id") { return(NULL) }
  #   bids_tabular_colname(name = nm,
  #                        descriptors = list(Description = "no description"))
  # }))
  # impl@header <- header
  #
  # impl
}

# bids_dataset_description()


