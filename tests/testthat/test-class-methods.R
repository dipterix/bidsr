test_that("class methods receive the decorated class definition", {
  TestClassMethods <- new_bids_class(
    name = "TestClassMethods",
    methods = list(
      tag = "value",
      get_tag = function(cls) { cls$tag },
      format = function(cls, ...) { cls$tag }
    )
  )
  x <- TestClassMethods()
  expect_identical(x$get_tag(), "value")
  expect_identical(format(x), "value")
})
