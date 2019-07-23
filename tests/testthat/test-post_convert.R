library(chemspiderapi)

context("post_convert")

test_that("post_convert() returns an NA vector if input is wrong", {
  expect_error(suppressWarnings(post_convert(input = "Not an InChIKey",
                                             inputFormat = "InChIKey",
                                             outputFormat = "SMILES",
                                             apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))))
})

test_that("post_convert() returns an NA_character_ vector if input format is wrong", {
  expect_error(suppressWarnings(post_convert(input = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N",
                                             inputFormat = "Not an input format",
                                             outputFormat = "SMILES",
                                             apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))))
})

test_that("post_convert() returns an NA_character_ vector if output format is wrong", {
  expect_error(suppressWarnings(post_convert(input = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N",
                                             inputFormat = "InChIKey",
                                             outputFormat = "Not an output format",
                                             apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))))
})

test_that("post_convert() returns an NA_character_ vector if apikey is wrong", {
  expect_error(suppressWarnings(post_convert(input = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N",
                                             inputFormat = "InChIKey",
                                             outputFormat = "SMILES",
                                             apikey = "A wrong apikey")))
})
