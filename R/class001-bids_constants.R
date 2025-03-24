# BIDS follows RFC 2119 and has three requirement levels: OPTIONAL, RECOMMENDED and REQUIRED. In the schema, we use optional, recommended and required.
#
# A rule interpreter (validator) is expected to treat: - missing REQUIRED data/metadata as an error, - missing RECOMMENDED data/metadata as a warning, - and silently pass over missing OPTIONAL data.
#
# BIDS also defines a level DEPRECATED, rendered in the schema as deprecated, and corresponding to a warning if the data/metadata is present.
BIDS_REQUIREMENT_LEVEL <- c("optional", "recommended", "required", "deprecated")

BIDS_ENTITY_REQUIREMENT_OPTIONS <- c("optional", "required", "prohibited")
BIDS_ENTITY_VALUE_TYPES <- c("label", "index", "any")

BIDS_SCHEMA_OBJECT_GROUP_TERM_TYPES <- c("general", "name-value", "values", "formats", "files")

# Default BIDS folder depth
BIDS_MAP_MAX_DEPTH <- function() {
  depth <- as.integer(getOption("bidsr.map.seach_depth", Sys.getenv("BIDS_MAP_MAX_DEPTH", "29")))
  if(length(depth) != 1 || is.na(depth) || !is.numeric(depth) || is.infinite(depth)) {
    depth <- 29L
  }  else if (depth < 0) {
    depth <- 0L
  }
  depth
}

DEFAULT_GENERATED_BY <- function() {
  desc <- read.dcf(system.file("DESCRIPTION", package = "bidsr"))
  desc <- structure(names = colnames(desc), as.list(desc))
  bids_dataset_generated_by(
    Name = as.character(desc$Package),
    Version = as.character(desc$Version),
    Description = paste(desc$Description, collapse = "\n"),
    CodeURL = as.character(desc$URL)
  )
}


