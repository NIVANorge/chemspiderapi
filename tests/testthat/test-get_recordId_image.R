library(chemspiderapi)

context("get_recordId_image")

test_that("get_recordId_image() fails if no recordId is provided.", {
  expect_error(
    get_recordId_image()
  )
})

test_that("get_recordId_image() fails if a NULL recordId is provided.", {
  expect_error(
    get_recordId_image(recordId = NULL)
  )
})

test_that("get_recordId_image() fails if a recordId is not a numeric vector.", {
  expect_error(
    get_recordId_image(recordId = "recordId")
  )
})

test_that("get_recordId_image() fails if multiple recordIds are provided.", {
  expect_error(
    get_recordId_image(recordId = c("123", "456"))
  )
})

test_that("get_recordId_image() fails if no API key is provided.", {
  expect_error(
    get_recordId_image(recordId = 2424L)
  )
})

test_that("get_recordId_image() fails if NULL is provided as API key.", {
  expect_error(
    get_recordId_image(recordId = 2424L, apikey = NULL)
  )
})

test_that("get_recordId_image() fails if more than one API key is provided.", {
  expect_error(
    get_recordId_image(recordId = 2424L, apikey = c("API key one", "API key two"))
  )
})

test_that("get_recordId_image() fails if a numeric API key is provided.", {
  expect_error(
    get_recordId_image(recordId = 2424L, apikey = 1234567890)
  )
})

test_that("get_recordId_image() fails if a logical API key is provided.", {
  expect_error(
    get_recordId_image(recordId = 2424L, apikey = TRUE)
  )
})

test_that("get_recordId_image() fails if a non 32-character length API key is provided.", {
  expect_error(
    get_recordId_image(recordId = 2424L, apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})
