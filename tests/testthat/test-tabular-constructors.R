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

test_that("tabular classes accept an abstract parent", {
  TestAbstractParent <- new_bids_tabular_class(
    table_name = "abstract_parent",
    parent = BIDSClassBase
  )
  x <- TestAbstractParent(content = data.frame(a = 1))
  expect_s7_class(x, TestAbstractParent)
  expect_identical(x@content$a, 1)
})

test_that("replacing tabular content keeps column descriptors", {
  x <- BIDSTabular(
    content = data.frame(a = 1, b = 2),
    meta = list(a = list(Description = "A"), b = list(Description = "B"))
  )
  x$content <- data.frame(a = 2, c = 3)
  expect_identical(x$meta$columns$a$Description, "A")
  expect_identical(x$meta$columns$b$Description, "B")
  expect_true("c" %in% names(x$meta$columns))

  y <- BIDSTabularParticipants(
    content = data.frame(participant_id = "sub-1", z = 1),
    meta = list(z = list(Description = "Z"))
  )
  y$content <- data.frame(participant_id = "sub-2", z = 2)
  expect_identical(y$meta$columns$z$Description, "Z")
})
