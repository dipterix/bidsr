# DIPSAUS DEBUG START
# library(testthat)
# devtools::load_all()

schema <- bids_schema("1.10.1")

test_that("parse entities - basic", {

  bids_entity_file_registry$clear_all()
  current_count <- bids_entity_file_registry$count()

  path_str <- "anat/sub-01_task-def_ses-abc_acq-ghi_T1w.nii.gz"

  parsed <- parse_path_bids_entity(path_str, auto_cache = TRUE)

  expect_true(S7::S7_inherits(parsed, BIDSEntityFile))

  expect_equal(as.character(format(parsed)), "anat/sub-01_ses-abc_task-def_acq-ghi_T1w.nii.gz")

  expect_equal(parsed@data_type, "anat")
  expect_equal(parsed@suffix, "T1w")

  expect_equal(parsed@identifier, "anat/t1w")

  # Make sure `auto_cache` is ON
  expect_equal(bids_entity_file_registry$count(), current_count + 1)

})

test_that("parse entities without datatype", {

  # Using schema key
  bids_version <- current_bids_version()
  schema <- bids_schema()
  # on.exit({
  #   use_bids_version(bids_version)
  # }, add = TRUE, after = TRUE)

  bids_entity_file_registry$clear_all()
  current_count <- bids_entity_file_registry$count()

  # ---- 1. No explicit datatype --------------------------------
  path_str <- "ses-01/sub-01_task-def_ses-abc_acq-ghi_T1w.nii.gz"

  parsed <- parse_path_bids_entity(path_str, auto_cache = TRUE)

  expect_true(S7::S7_inherits(parsed, BIDSEntityFile))

  expect_equal(as.character(format(parsed)), "ses-01/sub-01_ses-abc_task-def_acq-ghi_T1w.nii.gz")

  expect_equal(parsed@data_type, "_root")
  expect_equal(parsed@suffix, "T1w")

  expect_equal(parsed@identifier, "_root/t1w")

  # Make sure `auto_cache` is turned off internally
  expect_equal(bids_entity_file_registry$count(), current_count)

  # ---- 2. No explicit datatype buy schema key is provided -------------------

  schema_key <- "rules.files.raw.anat.nonparametric"
  file_rule <- schema$original[schema_key]
  expect_true(is.list(file_rule) && length(file_rule) > 0)

  # subject is required
  path_str <- "task-def_ses-abc_acq-ghi_T1w.nii.gz"
  expect_error(parse_path_bids_entity(path_str, auto_cache = TRUE, schema_key = schema_key))

  expect_equal(bids_entity_file_registry$count(), current_count + 1)

  path_str <- "sub-01_task-def_ses-abc_acq-ghi_T1w.nii.gz"
  parsed <- parse_path_bids_entity(path_str, auto_cache = TRUE, schema_key = schema_key)

  # datatype is inferred from schema key
  expect_equal(parsed@data_type, "anat")
  expect_equal(parsed@suffix, "T1w")
  expect_equal(as.character(format(parsed)), "./sub-01_ses-abc_task-def_acq-ghi_T1w.nii.gz")
  expect_equal(parsed@identifier, "anat/t1w")

  expect_equal(bids_entity_file_registry$count(), current_count + 1)
})
