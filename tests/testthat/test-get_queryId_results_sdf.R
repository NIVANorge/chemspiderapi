library(chemspiderapi)

context("get_queryId_results_sdf")

test_that("get_queryId_results_sdf() fails if no queryId is provided.", {
  expect_error(
    get_queryId_results_sdf()
  )
})

test_that("get_queryId_results_sdf() fails if a NULL queryId is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = NULL)
  )
})

test_that("get_queryId_results_sdf() fails if a queryId is not a character vector.", {
  expect_error(
    get_queryId_results_sdf(queryId = 123)
  )
})

test_that("get_queryId_results_sdf() fails if multiple queryIds are provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = c("0c98889f-5e8b-4974-aabb-31ab28c54261", "0c98889f-5e8b-4974-aabb-31ab28c54261"))
  )
})

test_that("get_queryId_results_sdf() fails if a queryId is too long.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261N")
  )
})

test_that("get_queryId_results_sdf() fails if a queryId is not hyphen divided into five parts.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb31ab28c54261N")
  )
})

test_that("get_queryId_results_sdf() fails if the first part a queryId is of the wrong length.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889fN-5e8-4974-aabb-31ab28c54261")
  )
})

test_that("get_queryId_results_sdf() fails if the second part a queryId is of the wrong length.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8bN-4974-aab-31ab28c54261")
  )
})

test_that("get_queryId_results_sdf() fails if the third part a queryId is of the wrong length.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974N-aab-31ab28c54261")
  )
})

test_that("get_queryId_results_sdf() fails if the fourth part a queryId is of the wrong length.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabbN-31ab28c5426")
  )
})

test_that("get_queryId_results_sdf() fails if no status is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261")
  )
})

test_that("get_queryId_results_sdf() fails if a NULL status is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = NULL)
  )
})

test_that("get_queryId_results_sdf() fails if a multiple status are provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = c("Complete", "Complete"))
  )
})

test_that("get_queryId_results_sdf() fails if a non-character status is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = 123)
  )
})

test_that("get_queryId_results_sdf() fails if the status is not complete.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Incomplete")
  )
})

test_that("get_queryId_results_sdf() fails if no API key is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete")
  )
})

test_that("get_queryId_results_sdf() fails if NULL is provided as API key.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = NULL)
  )
})

test_that("get_queryId_results_sdf() fails if more than one API key is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = c("API key one", "API key two"))
  )
})

test_that("get_queryId_results_sdf() fails if a numeric API key is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = 1234567890)
  )
})

test_that("get_queryId_results_sdf() fails if a logical API key is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = TRUE)
  )
})

test_that("get_queryId_results_sdf() fails if a non 32-character length API key is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})
