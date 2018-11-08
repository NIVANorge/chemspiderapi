library(chemspiderapi)

context("post_inchikey")

test_that("post_inchikey() returns an NA_character_ vector if InChIKey is wrong", {
  expect_equal(post_inchikey(inchikey = " A wrong InChIKey", 
                             apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key")), 
               NA_character_)
})

test_that("post_inchikey() returns an NA_character_ vector if apikey is wrong", {
  expect_equal(post_inchikey(inchikey = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N", 
                             apikey = "A wrong apikey"), 
               NA_character_)
})
