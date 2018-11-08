library(chemspiderapi)

context("get_datasources")

test_that("get_datasources() returns an NA_character_ vector if apikey is wrong", {
  expect_equal(get_datasources(apikey = "A wrong apikey"), 
               NA_character_)
})
