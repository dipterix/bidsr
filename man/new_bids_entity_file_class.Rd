% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class020-bids_file_with_entities.R
\name{new_bids_entity_file_class}
\alias{new_bids_entity_file_class}
\title{Class generator for 'BIDS' file class with entities}
\usage{
new_bids_entity_file_class(
  name,
  data_type,
  suffix,
  schema_key = NA,
  bids_version = current_bids_version()
)
}
\arguments{
\item{name}{class name}

\item{data_type}{'BIDS' file data type}

\item{suffix}{file suffix}

\item{schema_key}{schema key if explicit entity rules are required}

\item{bids_version}{'BIDS' version to query the entity rules}
}
\value{
A class definition with proper entity constraints according to
\code{data_type}-\code{suffix} combinations, or a specific \code{schema_key}.
The function rarely needs to be called directly unless the schema key is
missing from the specification.
}
\description{
Low-level function to generate file name definitions with entity
constraints; use \code{\link{parse_path_bids_entity}} instead.
The specification is at
\url{https://bids-specification.readthedocs.io/en/stable/common-principles.html#filenames}.
}
\examples{


# see full table at BIDS specification
# en/stable/appendices/entity-table.html#behavioral-data
#
# generate class definition for "Behavioral Data"
# Entity: Subject Session Task Acquisition Run Recording
# Format:
#   sub-<label> ses-<label> task-<label>
#   acq-<label> run-<index> recording-<label>
# suffix: events
# requirement: REQUIRED OPTIONAL REQUIRED OPTIONAL OPTIONAL
#


# ---- Basic usage ----------------------------------------
behavior_event_file_def <- new_bids_entity_file_class(
  name = "BIDSEntityFile_beh_events",
  data_type = "beh",
  suffix = "events"
)

file1 <- behavior_event_file_def(
  parent_directory = "sub-001/beh",
  sub = "001", task = "test", .extension = "tsv")

print(file1)

file.path("root/to/path", file1)

# How the entities are parsed?
file1$description

# get entity values
file1$get_bids_entity("task")

# parent directory
file1$parent_directory

file1$entities$run$value

# set entity values
file1$entities$run <- 2
file1$entities$run$index_format <- "\%03d"

file1$entities$blahblah <- "haha"

file1

# Relaxed entity rules generated from schema
# `rules.files.raw.task.events` and
# `rules.files.deriv.preprocessed_data.task_events_common`
get_bids_entity_rules(file1)

# ---- Using BIDS schema key for specific version ------------------------
bids_version <- "1.10.1"
behavior_event_file_def <- new_bids_entity_file_class(
  name = "BIDSEntityFile_beh_events",
  data_type = "beh",
  suffix = "events",
  schema_key = "rules.files.raw.task.events",
  bids_version = bids_version
)

file2 <- behavior_event_file_def(
  parent_directory = "sub-001/beh",
  sub = "001", task = "test", .extension = "tsv")

file2$description

# `desc` is no longer listed in the rules here
get_bids_entity_rules(file2)



}
\author{
Zhengjia Wang
}
