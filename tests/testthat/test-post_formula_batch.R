library(chemspiderapi)

context("post_formula_batch")

test_that("post_formula_batch() fails if no formulas are provided.", {
  expect_error(
    post_formula_batch()
  )
})

test_that("post_formula_batch() fails if NULL is provided as formulas.", {
  expect_error(
    post_formula_batch(formulas = NULL)
  )
})

test_that("post_formula_batch() fails if only a single formula is provided.", {
  expect_error(
    post_formula_batch(formulas = "C8H10N4O2")
  )
})

test_that("post_formula_batch() fails if more than 100 formulas are provided.", {
  expect_error(
    post_formula_batch(formulas = rep("C8H10N4O2", times = 101))
  )
})

test_that("post_formula_batch() fails if a non-character formulas are provided.", {
  expect_error(
    post_formula_batch(formulas = c(123, 456))
  )
})

test_that("post_formula_batch() fails if more than 20 data sources are provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"),
                       dataSources = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v"))
  )
})

test_that("post_formula_batch() fails if more than one orderBy is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), dataSources = NULL,
                orderBy = c("recordid", "massdefect"), orderDirection = NULL)
  )
})

test_that("post_formula_batch() fails if a false orderBy is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), dataSources = NULL,
                orderBy = "thewrongthing", orderDirection = NULL)
  )
})

test_that("post_formula_batch() fails if a non-character orderBy is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), dataSources = NULL,
                orderBy = 123, orderDirection = NULL)
  )
})

test_that("post_formula_batch() fails if more than one orderDirection is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), dataSources = NULL,
                orderBy = NULL, orderDirection = c("ascending", "descending"))
  )
})

test_that("post_formula_batch() fails if a non-character orderDirection is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), dataSources = NULL,
                orderBy = NULL, orderDirection = 123)
  )
})

test_that("post_formula_batch() fails if a false orderDirection is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), dataSources = NULL,
                orderBy = NULL, orderDirection = "thewrongthing")
  )
})

test_that("post_formula_batch() fails if no API key is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending")
  )
})

test_that("post_formula_batch() fails if NULL is provided as API key.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = NULL)
  )
})

test_that("post_formula_batch() fails if more than one API key is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = c("API key one", "API key two"))
  )
})

test_that("post_formula_batch() fails if a numeric API key is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = 1234567890)
  )
})

test_that("post_formula_batch() fails if a logical API key is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = TRUE)
  )
})

test_that("post_formula_batch() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_formula_batch(formulas = c("C8H10N4O2", "C10H14BrNO2"), dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})
