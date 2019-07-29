library(chemspiderapi)

context("check_inchi")

test_that("check_inchi() fails if no inchi is provided.", {
  expect_error(
    check_inchi()
    )
})

test_that("check_inchi() fails if NULL is provided as inchi.", {
  expect_error(
    check_inchi(inchi = NULL)
  )
})

test_that("check_inchi() fails if multiple inchi are provided.", {
  expect_error(
    check_inchi(inchi = c("InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3", "InChI=1S/C10H14BrNO2/c1-13-9-6-8(11)10(14-2)5-7(9)3-4-12/h5-6H,3-4,12H2,1-2H3"))
  )
})

test_that("check_inchi() fails if a non-character inchi is provided.", {
  expect_error(
    check_inchi(inchi = 123)
  )
})

test_that("check_inchi() fails if the inchi string is incomplete.", {
  expect_error(
    check_inchi(inchi = "C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3")
  )
})

test_that("check_inchi() warns if a non-standard inchi is provided.", {
  expect_warning(
    check_inchi(inchi = "InChI=1/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3")
  )
})

test_that("check_inchi() remains silent when correct inchi is provided.", {
  expect_silent(
    check_inchi(inchi = "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3")
  )
})
