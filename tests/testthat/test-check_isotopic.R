library(chemspiderapi)

context("check_isotopic")

test_that("check_isotopic() fails if no isotopic is provided.", {
  expect_error(
    .check_isotopic()
    )
})

test_that("check_isotopic() fails if NULL is provided as isotopic.", {
  expect_error(
    .check_isotopic(isotopic = NULL)
  )
})

test_that("check_isotopic() fails if multiple isotopic are provided.", {
  expect_error(
    .check_isotopic(isotopic = c("all", "labeled"))
  )
})

test_that("check_isotopic() fails if a non-character isotopic is provided.", {
  expect_error(
    .check_isotopic(isotopic = 123)
  )
})

test_that("check_isotopic() fails if a wrong isotopic is provided.", {
  expect_error(
    .check_isotopic(isotopic = "something")
  )
})

test_that("check_isotopic() remains silent when correct inchikey is provided.", {
  expect_silent(
    .check_isotopic(isotopic = "unlabeled")
  )
})
