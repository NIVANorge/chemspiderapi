library(chemspiderapi)

context("check_format")

test_that("check_format() fails if no input is provided.", {
  expect_error(
    check_format()
    )
})

test_that("check_format() fails if NULL is provided as input.", {
  expect_error(
    check_format(input = NULL)
  )
})

test_that("check_format() fails if multiple inputs are provided.", {
  expect_error(
    check_format(input = c("RYYVLZVUVIJVGH-UHFFFAOYSA-N", "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"))
  )
})

test_that("check_format() fails if a non-character input is provided.", {
  expect_error(
    check_format(input = 123)
  )
})

test_that("check_format() fails if no inputFormat is provided.", {
  expect_error(
    check_format(input = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", outputFormat = "SMILES")
  )
})

test_that("check_format() fails if NULL is provided as inputFormat.", {
  expect_error(
    check_format(input = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", inputFormat = NULL, outputFormat = "SMILES")
  )
})

test_that("check_format() fails if no outputFormat is provided.", {
  expect_error(
    check_format(input = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", inputFormat = "InChIKey")
  )
})

test_that("check_format() fails if NULL is provided as outputFormat.", {
  expect_error(
    check_format(input = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", inputFormat = "InChIKey", outputFormat = NULL)
  )
})

test_that("check_format() remains silent when correct inputs are provided.", {
  expect_silent(
    check_format(input = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", inputFormat = "InChIKey", outputFormat = "SMILES")
  )
})
