library(chemspiderapi)

context("check_smiles")

test_that("check_smiles() fails if no smiles is provided.", {
  expect_error(
    check_smiles()
  )
})

test_that("check_smiles() fails if a NULL smiles is provided.", {
  expect_error(
    check_smiles(smiles = NULL)
  )
})

test_that("check_smiles() fails if smiles is not a character vector.", {
  expect_error(
    check_smiles(smiles = 123)
  )
})

test_that("check_smiles() fails if multiple smiles are provided", {
  expect_error(
    check_smiles(smiles = c("CN1C=NC2=C1C(=O)N(C(=O)N2C)C", "COc1cc(CCN)c(OC)cc1Br"))
  )
})

test_that("check_smiles() remains silent when the correct smiles is provided.", {
  expect_silent(
    check_smiles(smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C")
  )
})
