library(chemspiderapi)

context("check_fields")

test_that("check_fields() fails if no character input is provided.", {
  expect_error(
    .check_fields(fields = 1)
    )
})

test_that("check_fields() fails if the wrong character string is provided as input.", {
  expect_error(
    .check_fields(fields = "Something")
  )
})

test_that("check_fields() remains silent when a correct field is supplied.", {
  expect_silent(
    .check_fields(fields = "Formula")
  )
})

test_that("check_fields() remains silent when correct fields are supplied.", {
  expect_silent(
    .check_fields(fields = c("Formula", "InChIKey"))
  )
})
