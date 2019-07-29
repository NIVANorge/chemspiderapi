library(chemspiderapi)

context("check_inchikey")

test_that("check_inchikey() fails if no inchikey is provided.", {
  expect_error(
    check_inchikey()
    )
})

test_that("check_inchikey() fails if NULL is provided as inchikey.", {
  expect_error(
    check_inchikey(inchikey = NULL)
  )
})

test_that("check_inchikey() fails if multiple inchikey are provided.", {
  expect_error(
    check_inchikey(inchikey = c("RYYVLZVUVIJVGH-UHFFFAOYSA-N", "YMHOBZXQZVXHBM-UHFFFAOYSA-N"))
  )
})

test_that("check_inchikey() fails if a non-character inchikey is provided.", {
  expect_error(
    check_inchikey(inchikey = 123)
  )
})

test_that("check_inchikey() fails if a single-string inchikey is provided.", {
  expect_error(
    check_inchikey(inchikey = "RYYVLZVUVIJVGHUHFFFAOYSAN")
  )
})

test_that("check_inchikey() fails if a three part inchikey has the wrong length.", {
  expect_error(
    check_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-NN")
  )
})

test_that("check_inchikey() fails if a two part inchikey has the wrong length.", {
  expect_error(
    check_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSAN")
  )
})

test_that("check_inchikey() fails if the first part of an inchikey has the wrong length.", {
  expect_error(
    check_inchikey(inchikey = "RYYVLZVUVIJVGHN-UHFFFAOYS-N")
  )
})

test_that("check_inchikey() fails if the second part of an inchikey has the wrong length.", {
  expect_error(
    check_inchikey(inchikey = "RYYVLZVUVIJVG-UHFFFAOYSAN-N")
  )
})

test_that("check_inchikey() fails if the third part of an inchikey has the wrong length.", {
  expect_error(
    check_inchikey(inchikey = "RYYVLZVUVIJVG-UHFFFAOYSA-NN")
  )
})

test_that("check_inchikey() warns if a non-standard inchikey is provided.", {
  expect_warning(
    check_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYXA-N")
  )
})

test_that("check_inchikey() remains silent when correct inchikey is provided.", {
  expect_silent(
    check_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N")
  )
})
