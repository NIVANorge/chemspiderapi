library(chemspiderapi)

context("check_recordId")

test_that("check_recordId() fails if no recordId is provided.", {
  expect_error(
    .check_recordId()
  )
})

test_that("check_recordId() fails if a NULL recordId is provided.", {
  expect_error(
    .check_recordId(recordId = NULL)
  )
})

test_that("check_recordId() fails if a recordId is not a numeric vector.", {
  expect_error(
    .check_recordId(recordId = "recordId")
  )
})

test_that("check_recordId() fails if multiple recordIds are provided.", {
  expect_error(
    .check_recordId(recordId = c("123", "456"))
  )
})

test_that("check_recordId() issues a warning when a non-integer recordId is provided.", {
  expect_warning(
    .check_recordId(recordId = 2424)
  )
})

test_that("check_recordId() remains silent when the correct recordId is provided.", {
  expect_silent(
    .check_recordId(recordId = 2424L)
  )
})
