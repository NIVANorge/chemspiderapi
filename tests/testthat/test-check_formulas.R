library(chemspiderapi)

context("check_formulas")

test_that("check_formulas() fails if no formulas are provided.", {
  expect_error(
    .check_formulas()
    )
})

test_that("check_formulas() fails if NULL is provided as formulas.", {
  expect_error(
    .check_formulas(formulas = NULL)
  )
})

test_that("check_formulas() fails if only a single formula is provided.", {
  expect_error(
    .check_formulas(formulas = "C8H10N4O2")
  )
})

test_that("check_formulas() fails if more than 100 formulas are provided.", {
  expect_error(
    .check_formulas(formulas = rep("C8H10N4O2", times = 101))
  )
})

test_that("check_formulas() fails if a non-character formulas are provided.", {
  expect_error(
    .check_formulas(formulas = c(123, 456))
  )
})

test_that("check_formulas() remains silent when correct input is provided.", {
  expect_silent(
    .check_formulas(formulas = c("C8H10N4O2", "C10H14BrNO2"))
  )
})
