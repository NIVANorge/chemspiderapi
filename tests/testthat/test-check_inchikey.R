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




test_that("check_inchikey() fails if the inchikey string is incomplete.", {
  expect_error(
    check_inchikey(inchikey = "C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3")
  )
})

test_that("check_inchikey() warns if a non-standard inchikey is provided.", {
  expect_warning(
    check_inchikey(inchikey = "InChI=1/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3")
  )
})

test_that("check_inchikey() remains silent when correct inchikey is provided.", {
  expect_silent(
    check_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N")
  )
})
