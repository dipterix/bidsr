% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aa-generics-resolve_bids_path.R
\name{resolve_bids_path}
\alias{resolve_bids_path}
\title{Resolve path of a 'BIDS' object}
\usage{
resolve_bids_path(x, ...)
}
\arguments{
\item{x}{'BIDS' object such as project or subject}

\item{...}{passed to generic methods}
}
\value{
A character of the resolved path
}
\description{
Resolve path of a 'BIDS' object
}
\examples{

# ---- BIDS project ------------------------------------------------


# This example needs extra demo files
# Run `download_bids_examples()` first
examples <- download_bids_examples(test = TRUE)

if(!isFALSE(examples)) {

  project_path <- file.path(examples, "ieeg_epilepsy_ecog")

  project <- BIDSProject(
    path = project_path,
    raw_data_relpath = ".",
    derivative_data_relpath = "derivatives"
  )

  # project root
  resolve_bids_path(project, storage = "root")

  # raw-data directory
  resolve_bids_path(project, storage = "raw")

  # source-data directory
  resolve_bids_path(project, storage = "source")

  # derivatives directory
  resolve_bids_path(project, storage = "derivative")

  # get relative directory to project root
  resolve_bids_path(project, storage = "derivative",
                    relative_to_project = TRUE)

}

# ---- BIDS subject ------------------------------------------------


# This example needs extra demo files
# Run `download_bids_examples()` first
examples <- download_bids_examples(test = TRUE)

if(!isFALSE(examples)) {

  project_path <- file.path(examples, "ieeg_epilepsy_ecog")

  subject <- BIDSSubject(project = project_path,
                          subject_code = "ecog01")

  # raw-data directory
  resolve_bids_path(subject, storage = "raw")

  # source-data directory
  resolve_bids_path(subject, storage = "source")

  # derivatives directory to freesurfer
  resolve_bids_path(subject, storage = "derivative",
                    prefix = "freesurfer")

  # get relative directory to project root
  resolve_bids_path(subject, storage = "raw",
                    relative_to_project = TRUE)

}

# ---- BIDS URI ----------------------------------------------------

# create a BIDS URI
uri <- BIDSURI("bids::sub-01/fmap/sub-01_dir-AP_epi.nii.gz")

# resolving a BIDS URI requires dataset_description.json
data_description <- get_bids_dataset_description(
  parent_directory = "/path/to/BIDS/folder",
  Name = "A dummy experiments",
  BIDSVersion = "1.6.0",

  DatasetLinks = list(
    "deriv1" = "derivatives/derivative1",
    "phantoms" = "file:///data/phantoms"
  )
)

resolve_bids_path(uri, data_description)

}
\author{
Zhengjia Wang
}
