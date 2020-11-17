library(chemspiderapi)

context("check_coerce")

test_that("check_coerce() fails when non-logical input is provided", {
  expect_error(
    .check_coerce(coerce = "test")
  )
})

test_that("check_coerce() fails when non-logical input is provided", {
  expect_error(
    .check_coerce(coerce = 123)
  )
})

test_that("check_coerce() fails when non-logical input is provided", {
  expect_error(
    .check_coerce(coerce = 123L)
  )
})

test_that("check_coerce() fails when no input is provided", {
  expect_error(
    .check_coerce(coerce = NULL)
  )
})

test_that("check_coerce() fails when multiple inputs are provided", {
  expect_error(
    .check_coerce(coerce = c(TRUE, FALSE))
  )
})

test_that("check_coerce() remains silent when correct input is provided", {
  expect_silent(
    .check_coerce(coerce = TRUE)
  )
})

test_that("check_coerce() remains silent when correct input is provided", {
  expect_silent(
    .check_coerce(coerce = FALSE)
  )
})
