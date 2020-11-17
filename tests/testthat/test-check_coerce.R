library(chemspiderapi)

context("check_coerce")

test_that("check_coerce() fails when non-logical input is provided", {
  coerce <- "test"
  expect_error(
    .check_coerce(coerce)
  )
})

test_that("check_coerce() fails when non-logical input is provided", {
  coerce <- 123
  expect_error(
    .check_coerce(coerce)
  )
})

test_that("check_coerce() fails when non-logical input is provided", {
  coerce <- 123L
  expect_error(
    .check_coerce(coerce)
  )
})

test_that("check_coerce() fails when no input is provided", {
  coerce <- NULL
  expect_error(
    .check_coerce(coerce)
  )
})

test_that("check_coerce() fails when multiple inputs are provided", {
  coerce <- c(TRUE, FALSE)
  expect_error(
    .check_coerce(coerce)
  )
})

test_that("check_coerce() remains silent when correct input is provided", {
  coerce <- TRUE
  expect_silent(
    .check_coerce(coerce)
  )
})

test_that("check_coerce() remains silent when correct input is provided", {
  coerce <- FALSE
  expect_silent(
    .check_coerce(coerce)
  )
})
