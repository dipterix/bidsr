test_that("public property factories defer empty defaults", {
  properties <- list(
    named = expect_no_condition(bids_property_named_list(name = "named")),
    entities = expect_no_condition(bids_property_entity_list(
      name = "entities"
    )),
    columns = expect_no_condition(bids_property_tabular_column_descriptor_list(
      name = "columns"
    )),
    frame = expect_no_condition(bids_property_data_frame(name = "frame"))
  )
  TestDefaults <- do.call(
    new_bids_class,
    list(name = "TestDefaults", properties = properties)
  )
  x <- TestDefaults()
  expect_type(x@named, "list")
  expect_length(x@named, 0)
  expect_type(x@entities, "list")
  expect_length(x@entities, 0)
  expect_type(x@columns, "list")
  expect_length(x@columns, 0)
  expect_identical(x@frame, data.frame())
})

test_that("data frame properties use class defaults and preserve explicit defaults", {
  table_class <- S7::new_S3_class(
    class = "data.table",
    constructor = \(.data = 1L) data.table::data.table(value = .data)
  )
  properties <- list(
    table = bids_property_data_frame(name = "table", class = table_class),
    frame = bids_property_data_frame(
      name = "frame",
      default = quote(data.frame(value = 2L))
    )
  )
  TestFrames <- do.call(
    new_bids_class,
    list(name = "TestFrames", properties = properties)
  )
  x <- TestFrames()
  expect_s3_class(x@table, "data.table")
  expect_identical(x@table$value, 1L)
  expect_identical(x@frame, data.frame(value = 2L))
})

test_that("entity defaults preserve list conversion", {
  properties <- list(
    entities = expect_no_condition(bids_property_entity_list(
      name = "entities",
      default = c(subject = "001")
    )),
    quoted = expect_no_condition(bids_property_entity_list(
      name = "quoted",
      default = quote(list(subject = "002"))
    ))
  )
  TestEntities <- do.call(
    new_bids_class,
    list(name = "TestEntities", properties = properties)
  )
  x <- TestEntities()
  expect_identical(x@entities$subject@value, "001")
  expect_identical(x@quoted$subject@value, "002")
})
