library(chemspiderapi)

context("check_dataSources")

test_that("check_dataSources() fails if more than 20 data sources are provided.", {
  expect_error(
    check_dataSources(dataSources = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v"))
    )
})

test_that("check_dataSources() is silent if a single data source is provided.", {
  expect_silent(
    check_dataSources(dataSources = "PubChem")
  )
})

test_that("check_dataSources() stays silent if two data sources are provided.", {
  expect_silent(
    check_dataSources(dataSources = c("PubChem", "ChEBI"))
  )
})
