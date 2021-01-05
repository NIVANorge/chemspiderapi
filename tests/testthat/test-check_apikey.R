library(chemspiderapi)

context("check_apikey")

test_that("check_apikey() fails if no API key is provided.", {
  expect_error(
    .check_apikey()
  )
})

test_that("check_apikey() fails if NULL is provided as API key.", {
  expect_error(
    .check_apikey(apikey = NULL)
  )
})

test_that("check_apikey() fails if more than one API key is provided.", {
  expect_error(
    .check_apikey(apikey = c("API key one", "API key two"))
    )
})

test_that("check_apikey() fails if a numeric API key is provided.", {
  expect_error(
    .check_apikey(apikey = 1234567890)
  )
})

test_that("check_apikey() fails if a logical API key is provided.", {
  expect_error(
    .check_apikey(apikey = TRUE)
  )
})

test_that("check_apikey() fails if a non 32-character length API key is provided.", {
  expect_error(
    .check_apikey(apikey = "abcdefghijklmnopqrstuvqxyz")
    )
})

test_that("check_apikey() succeeds if a 32-character length API key is provided.", {
  expect_silent(
    .check_apikey(apikey = "abcdefghijklmnopqrstuvqxyz123456")
  )
})
