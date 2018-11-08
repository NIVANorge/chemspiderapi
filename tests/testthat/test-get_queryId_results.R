library(chemspiderapi)

context("get_queryId_results")

test_that("get_queryId_results() returns an NA_integer_ vector if query ID is wrong", {
  expect_equal(get_queryId_results(queryId = "A wrong query ID",
                                   status = "Complete",
                                   apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key")), 
               NA_integer_)
})

test_that("get_queryId_results() returns an NA_integer_ vector if apikey is wrong", {
  expect_equal(get_queryId_results(queryId = "abcdefgh-abcd-abcd-abcd-abcdefghijkl", 
                                   status = "Complete",
                                   apikey = "A wrong apikey"), 
               NA_integer_)
})

test_that("get_queryId_results() returns an NA_integer_ vector if status is Incomplete", {
  expect_equal(get_queryId_results(queryId = "abcdefgh-abcd-abcd-abcd-abcdefghijkl", 
                                   status = "Incomplete",
                                   apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key")), 
               NA_integer_)
})
