library(chemspiderapi)

context("check_complexity")

test_that("check_complexity() fails if more than one complexity is provided.", {
  expect_error(
    .check_complexity(complexity = c("any", "single", "multiple"))
    )
})

test_that("check_complexity() fails if a wrong character string is provided.", {
  expect_error(
    .check_complexity(complexity = "something")
  )
})

test_that("check_complexity() fails if a numeric value is provided.", {
  expect_error(
    .check_complexity(complexity = 123)
  )
})


test_that("check_complexity() fails if a logical is provided.", {
  expect_error(
    .check_complexity(complexity = TRUE)
  )
})

test_that("check_complexity() remains silent if one correct complexity is provided.", {
  expect_silent(
    .check_complexity(complexity = "any")
  )
})
