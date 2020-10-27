library(chemspiderapi)

context("get_datasources")

test_that("get_datasources() returns nothing if API key length is wrong.", {
  expect_error(
    get_datasources(apikey = "A wrong API key")
    )
})

test_that("get_datasources() returns nothing if API key type is wrong.", {
  expect_error(
    get_datasources(apikey = 1234567890L)
    )
})

test_that("get_datasources() returns nothing if multiple API keys are provided.", {
  expect_error(
    get_datasources(apikey = c("A wrong API key", "Another wrong API key"))
    )
})
