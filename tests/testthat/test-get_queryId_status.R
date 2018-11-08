library(chemspiderapi)

context("get_queryId_status")

test_that("get_queryId_status() returns an NA_integer_ vector if query ID is wrong", {
  expect_equal(suppressWarnings(get_queryId_status(queryId = "A wrong query ID",
                                                   count = FALSE, message = FALSE,
                                                   apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))), 
               NA_character_)
})

test_that("get_queryId_status() returns an NA_character_ vector if apikey is wrong", {
  expect_equal(suppressWarnings(get_queryId_status(queryId = "abcdefgh-abcd-abcd-abcd-abcdefghijkl", 
                                                   count = FALSE, message = FALSE,
                                                   apikey = "A wrong apikey")), 
               NA_character_)
})
