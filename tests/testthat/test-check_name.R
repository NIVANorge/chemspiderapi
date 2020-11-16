library(chemspiderapi)

context("check_name")

test_that("check_name() fails if no name is provided.", {
  expect_error(
    .check_name()
  )
})

test_that("check_name() fails if NULL is provided as name.", {
  expect_error(
    .check_name(name = NULL)
  )
})

test_that("check_name() fails if a non-character name is provided.", {
  expect_error(
    .check_name(name = 123)
  )
})

test_that("check_name() fails if multiple names are provided.", {
  expect_error(
    .check_name(name = c("caffeine", "2c-b"))
  )
})

test_that("check_name() remains silent when correct name is provided.", {
  expect_silent(
    .check_name(name = "caffeine")
  )
})
