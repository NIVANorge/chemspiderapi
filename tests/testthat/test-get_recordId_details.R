library(chemspiderapi)

context("get_recordId_details")

test_that("get_recordId_details() returns an NA vector if record ID is wrong", {
  expect_equal(suppressWarnings(get_recordId_details(recordId = "A wrong query ID",
                                                     apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))), 
               NA)
})

test_that("get_recordId_details() returns an NA vector if apikey is wrong", {
  expect_equal(suppressWarnings(get_recordId_details(recordId = 2157L, 
                                                     apikey = "A wrong apikey")), 
               NA)
})
