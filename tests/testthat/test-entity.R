# DIPSAUS DEBUG START
# library(testthat)
# devtools::load_all()

schema <- bids_schema("1.10.1")

test_that("entity", {

  schema$rules$entities

  entity_str <- "anat/sub-01_ses-abc_task-def_acq-ghi_T1w.nii.gz"

  parsed <- parse_path_bids_entity(entity_str)
  get_bids_entity_rules(parsed)

  skip_on_cran()

})
