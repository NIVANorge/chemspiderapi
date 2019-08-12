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

test_that("check_format() fails if the inchi string is incomplete.", {
  expect_error(
    check_format(input = "C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3", inputFormat = "InChI", outputFormat = "SMILES")
  )
})

test_that("check_format() fails if smiles is not a character vector.", {
  expect_error(
    check_format(input = "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3", inputFormat = "SMILES", outputFormat = "InChIKey")
  )
})
