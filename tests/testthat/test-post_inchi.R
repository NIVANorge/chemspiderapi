library(chemspiderapi)

context("post_inchi")

test_that("post_inchi() fails if no inchi is provided.", {
  expect_error(
    post_inchi()
  )
})

test_that("post_inchi() fails if NULL is provided as inchi.", {
  expect_error(
    post_inchi(inchi = NULL)
  )
})

test_that("post_inchi() fails if multiple inchi are provided.", {
  expect_error(
    post_inchi(inchi = c("InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3", "InChI=1S/C10H14BrNO2/c1-13-9-6-8(11)10(14-2)5-7(9)3-4-12/h5-6H,3-4,12H2,1-2H3"))
  )
})

test_that("post_inchi() fails if a non-character inchi is provided.", {
  expect_error(
    post_inchi(inchi = 123)
  )
})

test_that("post_inchi() fails if the inchi string is incomplete.", {
  expect_error(
    post_inchi(inchi = "C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3")
  )
})

test_that("post_inchi() fails if no API key is provided.", {
  expect_error(
    post_inchi(inchi = "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3")
  )
})

test_that("post_inchi() fails if NULL is provided as API key.", {
  expect_error(
    post_inchi(inchi = "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3", apikey = NULL)
  )
})

test_that("post_inchi() fails if more than one API key is provided.", {
  expect_error(
    post_inchi(inchi = "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3", apikey = c("API key one", "API key two"))
  )
})

test_that("post_inchi() fails if a numeric API key is provided.", {
  expect_error(
    post_inchi(inchi = "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3", apikey = 1234567890)
  )
})

test_that("post_inchi() fails if a logical API key is provided.", {
  expect_error(
    post_inchi(inchi = "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3", apikey = TRUE)
  )
})

test_that("post_inchi() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_inchi(inchi = "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3", apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})
