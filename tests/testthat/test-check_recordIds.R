library(chemspiderapi)

context("check_recordIds")

test_that("check_recordIds() fails if no recordIds are provided.", {
  expect_error(
    .check_recordIds()
  )
})

test_that("check_recordIds() fails if NULL recordIds are provided.", {
  expect_error(
    .check_recordIds(recordIds = NULL)
  )
})

test_that("check_recordIds() fails if recordIds are not a numeric vector.", {
  expect_error(
    .check_recordIds(recordIds = c("recordId1", "recordId2"))
  )
})

test_that("check_recordIds() fails if only a single recordId is provided.", {
  expect_error(
    .check_recordIds(recordIds = 2424L)
  )
})

test_that("check_recordIds() fails if more than 100 recordIds are provided.", {
  expect_error(
    .check_recordIds(recordIds = 1:101)
  )
})


test_that("check_recordIds() issues a warning when a non-integer recordIds is provided.", {
  expect_warning(
    .check_recordIds(recordIds = c(2424, 2345))
  )
})

test_that("check_recordIds() remains silent when the correct recordIds is provided.", {
  expect_silent(
    .check_recordIds(recordIds = c(2424L, 2345L))
  )
})
