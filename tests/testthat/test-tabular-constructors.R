test_that("tabular subclasses construct their declared parent", {
  x <- BIDSTabularParticipants(content = data.frame(participant_id = "sub-001"))
  expect_s7_class(x, BIDSTabular)
  expect_identical(x@content$participant_id, "sub-001")

  TestTabular <- new_bids_tabular_class(
    table_name = "custom",
    lower_case_column_names = TRUE
  )
  custom <- TestTabular(content = data.frame(Participant_ID = "sub-002"))
  expect_s7_class(custom, TestTabular)
  expect_named(custom@content, "participant_id")
  expect_identical(custom@content$participant_id, "sub-002")
})
