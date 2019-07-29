library(chemspiderapi)

context("check_order")

test_that("check_order() fails if more than one orderBy is provided.", {
  expect_error(
    check_order(orderBy = c("recordid", "massdefect"), orderDirection = NULL)
  )
})


test_that("check_order() fails if a false orderBy is provided.", {
  expect_error(
    check_order(orderBy = "thewrongthing", orderDirection = NULL)
  )
})

test_that("check_order() fails if a non-character orderBy is provided.", {
  expect_error(
    check_order(orderBy = 123, orderDirection = NULL)
  )
})

test_that("check_order() fails if more than one orderDirection is provided.", {
  expect_error(
    check_order(orderBy = NULL, orderDirection = c("ascending", "descending"))
  )
})

test_that("check_order() fails if a non-character orderDirection is provided.", {
  expect_error(
    check_order(orderBy = NULL, orderDirection = 123)
  )
})

test_that("check_order() fails if a false orderDirection is provided.", {
  expect_error(
    check_order(orderBy = NULL, orderDirection = "thewrongthing")
  )
})


test_that("check_order() remains silent when the correct order is provided.", {
  expect_silent(
    check_order(orderBy = "recordId", orderDirection = "descending")
  )
})
