library(chemspiderapi)

context("check_status_code")

test_that("check_status_code() fails if status_code 400 is provided.", {
  expect_error(
    .check_status_code(status_code = 400L)
  )
})

test_that("check_status_code() fails if status_code 401 is provided.", {
  expect_error(
    .check_status_code(status_code = 401L)
  )
})

test_that("check_status_code() fails if status_code 404 is provided.", {
  expect_error(
    .check_status_code(status_code = 404L)
  )
})

test_that("check_status_code() fails if status_code 405 is provided.", {
  expect_error(
    .check_status_code(status_code = 405L)
  )
})

test_that("check_status_code() fails if status_code 413 is provided.", {
  expect_error(
    .check_status_code(status_code = 413L)
  )
})

test_that("check_status_code() fails if status_code 429 is provided.", {
  expect_error(
    .check_status_code(status_code = 429L)
  )
})

test_that("check_status_code() fails if status_code 500 is provided.", {
  expect_error(
    .check_status_code(status_code = 500L)
  )
})

test_that("check_status_code() fails if status_code 503 is provided.", {
  expect_error(
    .check_status_code(status_code = 503L)
  )
})

test_that("check_status_code() fails if an unknown status_code is provided.", {
  expect_error(
    .check_status_code(status_code = 600L)
  )
})

test_that("check_status_code() remains silent when status_code 200 is provided.", {
  expect_silent(
    .check_status_code(status_code = 200L)
  )
})
