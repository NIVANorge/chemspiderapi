library(chemspiderapi)

context("check_property")

test_that("check_property() fails if no property is provided.", {
  expect_error(
    check_property()
  )
})

test_that("check_property() fails if a NULL property is provided.", {
  expect_error(
    check_property(property = NULL)
  )
})

test_that("check_property() fails if multiple properties are provided.", {
  expect_error(
    check_property(property = c("formula", "molecularweight"))
  )
})

test_that("check_property() fails if a wrong property is provided.", {
  expect_error(
    check_property(property = "awrongproperty")
  )
})

test_that("check_property() fails if a non-character property is provided.", {
  expect_error(
    check_property(property = 123)
  )
})

test_that("check_property() remains silent when the correct property is provided.", {
  expect_silent(
    check_property(property = "formula")
  )
})
