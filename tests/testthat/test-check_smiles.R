library(chemspiderapi)

context("check_smiles")

test_that("check_smiles() fails if no smiles is provided.", {
  expect_error(
    .check_smiles()
  )
})

test_that("check_smiles() fails if a NULL smiles is provided.", {
  expect_error(
    .check_smiles(smiles = NULL)
  )
})

test_that("check_smiles() fails if smiles is not a character vector.", {
  expect_error(
    .check_smiles(smiles = 123)
  )
})

test_that("check_smiles() fails if an InChI string is provided.", {
  expect_error(
    .check_smiles(smiles = "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3")
  )
})

test_that("check_smiles() fails if multiple smiles are provided", {
  expect_error(
    .check_smiles(smiles = c("CN1C=NC2=C1C(=O)N(C(=O)N2C)C", "COc1cc(CCN)c(OC)cc1Br"))
  )
})

test_that("check_smiles() remains silent when the correct smiles is provided.", {
  expect_silent(
    .check_smiles(smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C")
  )
})
