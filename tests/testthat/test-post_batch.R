library(chemspiderapi)

context("post_batch")

test_that("post_batch() fails if no recordIds are provided.", {
  expect_error(
    post_batch(fields = "all", apikey = "apikey")
  )
})

test_that("post_batch() fails if NULL recordIds are provided.", {
  expect_error(
    post_batch(recordIds = NULL, fields = "all", apikey = "apikey")
  )
})

test_that("post_batch() fails if recordIds are not a numeric vector.", {
  expect_error(
    post_batch(recordIds = c("recordId1", "recordId2"), fields = "all", apikey = "apikey")
  )
})

test_that("post_batch() fails if only a single recordId is provided.", {
  expect_error(
    post_batch(recordIds = 2424L, fields = "all", apikey = "apikey")
  )
})

test_that("post_batch() fails if more than 100 recordIds are provided.", {
  expect_error(
    post_batch(recordIds = 1:101, fields = "all", apikey = "apikey")
  )
})

test_that("post_batch() fails if no character input is provided.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = 1)
  )
})

test_that("post_batch() fails if the wrong character string is provided as input.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "Something")
  )
})

test_that("post_batch() fails if no API key is provided.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "all")
  )
})

test_that("post_batch() fails if NULL is provided as API key.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "all", apikey = NULL)
  )
})

test_that("post_batch() fails if more than one API key is provided.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "all", apikey = c("API key one", "API key two"))
  )
})

test_that("post_batch() fails if a numeric API key is provided.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "all", apikey = 1234567890)
  )
})

test_that("post_batch() fails if a logical API key is provided.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "all", apikey = TRUE)
  )
})

test_that("post_batch() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "all", apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})
