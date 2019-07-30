library(chemspiderapi)

context("post_intrinsicproperty")

test_that("post_intrinsicproperty() fails if property is not formula and no mass is provided", {
  expect_error(
    post_intrinsicproperty(property = "molecularWeight", range = 0.002)
    )
})

test_that("post_intrinsicproperty() fails if property is not formula and no range is provided", {
  expect_error(
    post_intrinsicproperty(property = "molecularWeight", mass = 150)
  )
})

test_that("post_intrinsicproperty() fails if property is formula and no formula is provided", {
  expect_error(
    post_intrinsicproperty(property = "formula")
  )
})
