library(chemspiderapi)

context("check_start_and_count")

test_that("check_start_and_count() fails if start is not a numeric vector.", {
  expect_error(
    check_start_and_count(start = "start", count = NULL)
  )
})

test_that("check_start_and_count() fails if count is not a numeric vector.", {
  expect_error(
    check_start_and_count(start = NULL, count = "count")
  )
})

test_that("check_start_and_count() issues a warning if start is a double numeric vector.", {
  expect_warning(
    check_start_and_count(start = 1, count = NULL)
  )
})

test_that("check_start_and_count() issues a warning if count is a double numeric vector.", {
  expect_warning(
    check_start_and_count(start = NULL, count = 100)
  )
})

test_that("check_start_and_count() remains silent when the correct start and count are provided.", {
  expect_silent(
    check_start_and_count(start = 1L, count = 100L)
  )
})
