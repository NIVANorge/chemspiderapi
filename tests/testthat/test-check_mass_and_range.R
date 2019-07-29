library(chemspiderapi)

context("check_mass_and_range")

test_that("check_mass_and_range() fails if non-numeric mass is provided.", {
  expect_error(
    check_mass_and_range(mass = "hundredfifty", range = 0.002)
  )
})

test_that("check_mass_and_range() fails if non-numeric range is provided.", {
  expect_error(
    check_mass_and_range(mass = 150, range = "naughtpointzerozerotwo")
  )
})

test_that("check_mass_and_range() fails if mass is too small.", {
  expect_error(
    check_mass_and_range(mass = 0.1, range = 0.002)
  )
})

test_that("check_mass_and_range() fails if mass is too large.", {
  expect_error(
    check_mass_and_range(mass = 11100, range = 0.002)
  )
})

test_that("check_mass_and_range() fails if range is too small.", {
  expect_error(
    check_mass_and_range(mass = 150, range = 0.00001)
  )
})

test_that("check_mass_and_range() fails if range is too large.", {
  expect_error(
    check_mass_and_range(mass = 150, range = 1000)
  )
})

test_that("check_mass_and_range() fails if mass and range do not have the same length.", {
  expect_error(
    check_mass_and_range(mass = c(150, 140, 120), range = c(0.002, 0.001))
  )
})

test_that("check_mass_and_range() remains silent when correct mass and range is provided.", {
  expect_silent(
    check_mass_and_range(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002))
  )
})
