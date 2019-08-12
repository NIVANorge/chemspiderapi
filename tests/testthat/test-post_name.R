library(chemspiderapi)

context("post_name")

test_that("post_name() fails if no name is provided.", {
  expect_error(
    post_name()
  )
})

test_that("post_name() fails if NULL is provided as name.", {
  expect_error(
    post_name(name = NULL)
  )
})

test_that("post_name() fails if a non-character name is provided.", {
  expect_error(
    post_name(name = 123)
  )
})

test_that("post_name() fails if multiple names are provided.", {
  expect_error(
    post_name(name = c("caffeine", "2c-b"))
  )
})

test_that("post_name() fails if more than one orderBy is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = c("recordid", "massdefect"), orderDirection = NULL)
  )
})

test_that("post_name() fails if a false orderBy is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "thewrongthing", orderDirection = NULL)
  )
})

test_that("post_name() fails if a non-character orderBy is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = 123, orderDirection = NULL)
  )
})

test_that("post_name() fails if more than one orderDirection is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = NULL, orderDirection = c("ascending", "descending"))
  )
})

test_that("post_name() fails if a non-character orderDirection is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = NULL, orderDirection = 123)
  )
})

test_that("post_name() fails if a false orderDirection is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = NULL, orderDirection = "thewrongthing")
  )
})

test_that("post_name() fails if no API key is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "recordId", orderDirection = "ascending")
  )
})

test_that("post_name() fails if NULL is provided as API key.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "recordId", orderDirection = "ascending", apikey = NULL)
  )
})

test_that("post_name() fails if more than one API key is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "recordId", orderDirection = "ascending", apikey = c("API key one", "API key two"))
  )
})

test_that("post_name() fails if a numeric API key is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "recordId", orderDirection = "ascending", apikey = 1234567890)
  )
})

test_that("post_name() fails if a logical API key is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "recordId", orderDirection = "ascending", apikey = TRUE)
  )
})

test_that("post_name() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "recordId", orderDirection = "ascending", apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})
