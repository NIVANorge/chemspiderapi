library(chemspiderapi)

context("check_status")

test_that("check_status() fails if no status is provided.", {
  expect_error(
    check_status()
  )
})

test_that("check_status() fails if a NULL status is provided.", {
  expect_error(
    check_status(status = NULL)
  )
})

test_that("check_status() fails if a multiple status are provided.", {
  expect_error(
    check_status(status = c("Complete", "Complete"))
  )
})

test_that("check_status() fails if a non-character status is provided.", {
  expect_error(
    check_status(status = 123)
  )
})

test_that("check_status() fails if the status is not complete.", {
  expect_error(
    check_status(status = "Incomplete")
  )
})

test_that("check_status() remains silent when a complete status is provided.", {
  expect_silent(
    check_status(status = "Complete")
  )
})
