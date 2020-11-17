library(chemspiderapi)

context("check_simplify")

test_that("check_simplify() fails when non-logical input is provided", {
  simplify <- "test"
  expect_error(
    .check_simplify(simplify)
  )
})

test_that("check_simplify() fails when non-logical input is provided", {
  simplify <- 123
  expect_error(
    .check_simplify(simplify)
  )
})

test_that("check_simplify() fails when non-logical input is provided", {
  simplify <- 123L
  expect_error(
    .check_simplify(simplify)
  )
})

test_that("check_simplify() fails when no input is provided", {
  simplify <- NULL
  expect_error(
    .check_simplify(simplify)
  )
})

test_that("check_simplify() fails when multiple inputs are provided", {
  simplify <- c(TRUE, FALSE)
  expect_error(
    .check_simplify(simplify)
  )
})

test_that("check_simplify() remains silent when correct input is provided", {
  simplify <- TRUE
  expect_silent(
    .check_simplify(simplify)
  )
})

test_that("check_simplify() remains silent when correct input is provided", {
  simplify <- FALSE
  expect_silent(
    .check_simplify(simplify)
  )
})
