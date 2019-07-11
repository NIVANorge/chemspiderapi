library(chemspiderapi)

context("post_inchikey")

test_that("post_inchikey() fails pre-query if InChIKey is wrong", {
  expect_error(
    post_inchikey(inchikey = " A wrong InChIKey", apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))
    )
})

test_that("post_inchikey() fails pre-query if apikey is wrong", {
  expect_error(
    post_inchikey(inchikey = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N", apikey = "A wrong apikey")
    )
})
