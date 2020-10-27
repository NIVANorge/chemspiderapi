library(chemspiderapi)

context("check_result")

test_that("check_result() fails if no result is provided.", {
  expect_error(
    check_result()
    )
})

test_that("check_result() fails if no result is provided.", {
  expect_length(
    check_result(result = data.frame(column_1 = 1:10)), 
    10
  )
})
