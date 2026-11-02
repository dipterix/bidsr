test_that("BIDS map bracket methods dispatch on the map", {
  x <- BIDSMap()
  x[["key"]] <- 1
  expect_identical(x[["key"]], 1)
  expect_identical(x$key, 1)
})
