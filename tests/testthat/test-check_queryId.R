library(chemspiderapi)

context("check_queryId")

test_that("check_queryId() fails if no queryId is provided.", {
  expect_error(
    check_queryId()
  )
})

test_that("check_queryId() fails if a NULL queryId is provided.", {
  expect_error(
    check_queryId(queryId = NULL)
  )
})

test_that("check_queryId() fails if a queryId is not a character vector.", {
  expect_error(
    check_queryId(queryId = 123)
  )
})

test_that("check_queryId() fails if multiple queryIds are provided.", {
  expect_error(
    check_queryId(queryId = c("0c98889f-5e8b-4974-aabb-31ab28c54261", "0c98889f-5e8b-4974-aabb-31ab28c54261"))
  )
})

test_that("check_queryId() fails if a queryId is too long.", {
  expect_error(
    check_queryId(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261N")
  )
})

test_that("check_queryId() fails if a queryId is not hyphen divided into five parts.", {
  expect_error(
    check_queryId(queryId = "0c98889f-5e8b-4974-aabb31ab28c54261N")
  )
})

test_that("check_queryId() fails if the first part a queryId is of the wrong length.", {
  expect_error(
    check_queryId(queryId = "0c98889fN-5e8-4974-aabb-31ab28c54261")
  )
})

test_that("check_queryId() fails if the second part a queryId is of the wrong length.", {
  expect_error(
    check_queryId(queryId = "0c98889f-5e8bN-4974-aab-31ab28c54261")
  )
})

test_that("check_queryId() fails if the third part a queryId is of the wrong length.", {
  expect_error(
    check_queryId(queryId = "0c98889f-5e8b-4974N-aab-31ab28c54261")
  )
})

test_that("check_queryId() fails if the fourth part a queryId is of the wrong length.", {
  expect_error(
    check_queryId(queryId = "0c98889f-5e8b-4974-aabbN-31ab28c5426")
  )
})

test_that("check_queryId() remains silent when the correct queryId is provided.", {
  expect_silent(
    check_queryId(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261")
  )
})
