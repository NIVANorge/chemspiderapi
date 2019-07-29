library(chemspiderapi)

context("check_formula")

test_that("check_formula() fails if no formula is provided.", {
  expect_error(
    check_formula()
    )
})

test_that("check_formula() fails if NULL is provided as formula.", {
  expect_error(
    check_formula(formula = NULL)
  )
})

test_that("check_formula() fails if multiple formulas are provided.", {
  expect_error(
    check_formula(formula = c("C8H10N4O2", "C10H14BrNO2"))
  )
})

test_that("check_formula() fails if a non-character formula is provided.", {
  expect_error(
    check_formula(formula = 123)
  )
})

test_that("check_formula() remains silent when correct input is provided.", {
  expect_silent(
    check_formula(formula = "C8H10N4O2")
  )
})
