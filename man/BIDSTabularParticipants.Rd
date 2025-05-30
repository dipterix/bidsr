% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aa-generics-as_bids_tabular.R,
%   R/class031-bids_participants.R
\name{get_bids_participants}
\alias{get_bids_participants}
\alias{BIDSTabularParticipants}
\title{'BIDS' participant table class}
\usage{
get_bids_participants(x, ...)

BIDSTabularParticipants(content, meta = NULL)
}
\arguments{
\item{x}{R object such as file path, project instances, etc.}

\item{...}{passed to other methods or ignored}

\item{content, meta}{see \code{\link{BIDSTabular}}}
}
\value{
A \code{BIDSTabularParticipants} instance inheriting
\code{\link{BIDSTabular}}.
}
\description{
A tabular containing a list of participants and their demographics.
The class is a child class of \code{\link{BIDSTabular}}, hence see
the methods there.
The original specification is at
\url{https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html#participants-file}.
}
\examples{



# basic
tabular <- BIDSTabularParticipants(
  data.frame(
    participant_id = "sub-001"
  )
)
tabular


# Run `download_bids_examples()` first
examples <- download_bids_examples(test = TRUE)
if(!isFALSE(examples)) {

  file <- file.path(examples, "ieeg_epilepsy_ecog", "participants.tsv")

  # read tabular as BIDSTabularParticipants
  as_bids_tabular(file, cls = BIDSTabularParticipants)

  # convert existing tabular
  tabular <- BIDSTabular(
    data.frame(
      participant_id = "sub-001"
    )
  )
  tabular <- as_bids_tabular(tabular, cls = BIDSTabularParticipants)

  # save to tsv
  tsv <- file.path(tempdir(), "participants.tsv")
  paths <- save_bids_tabular(tabular, tsv)
  print(paths)

  # use base R to read
  read.table(tsv, header = TRUE, na.strings = "n/a")

  # get sidecar
  cat(readLines(paths$sidecar_path), sep = "\n")

  unlink(tsv)
  unlink(paths$sidecar_path)
}

}
\author{
Zhengjia Wang
}
