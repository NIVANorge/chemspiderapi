library(chemspiderapi)

context("post_validate_inchikey")

test_that("post_validate_inchikey() fails if no inchikey is provided.", {
  expect_error(
    post_validate_inchikey()
  )
})

test_that("post_validate_inchikey() fails if NULL is provided as inchikey.", {
  expect_error(
    post_validate_inchikey(inchikey = NULL)
  )
})

test_that("post_validate_inchikey() fails if multiple inchikey are provided.", {
  expect_error(
    post_validate_inchikey(inchikey = c("RYYVLZVUVIJVGH-UHFFFAOYSA-N", "YMHOBZXQZVXHBM-UHFFFAOYSA-N"))
  )
})

test_that("post_validate_inchikey() fails if a non-character inchikey is provided.", {
  expect_error(
    post_validate_inchikey(inchikey = 123)
  )
})

test_that("post_validate_inchikey() fails if a single-string inchikey is provided.", {
  expect_error(
    post_validate_inchikey(inchikey = "RYYVLZVUVIJVGHUHFFFAOYSAN")
  )
})

test_that("post_validate_inchikey() fails if a three part inchikey has the wrong length.", {
  expect_error(
    post_validate_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-NN")
  )
})

test_that("post_validate_inchikey() fails if a two part inchikey has the wrong length.", {
  expect_error(
    post_validate_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSAN")
  )
})

test_that("post_validate_inchikey() fails if the first part of an inchikey has the wrong length.", {
  expect_error(
    post_validate_inchikey(inchikey = "RYYVLZVUVIJVGHN-UHFFFAOYS-N")
  )
})

test_that("post_validate_inchikey() fails if no API key is provided.", {
  expect_error(
    post_validate_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N")
  )
})

test_that("post_validate_inchikey() fails if NULL is provided as API key.", {
  expect_error(
    post_validate_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", apikey = NULL)
  )
})

test_that("post_validate_inchikey() fails if more than one API key is provided.", {
  expect_error(
    post_validate_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", apikey = c("API key one", "API key two"))
  )
})

test_that("post_validate_inchikey() fails if a numeric API key is provided.", {
  expect_error(
    post_validate_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", apikey = 1234567890)
  )
})

test_that("post_validate_inchikey() fails if a logical API key is provided.", {
  expect_error(
    post_validate_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", apikey = TRUE)
  )
})

test_that("post_validate_inchikey() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_validate_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N", apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"valid\":true}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_VALIDATE_INCHIKEY_URL" = web$url())

test_that("post_validate_inchikey() returns a proper response.", {
  expect_type(
    post_validate_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N",
                           apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "logical"
  )
})

Sys.unsetenv("POST_VALIDATE_INCHIKEY_URL")

web$stop()

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(400L)$
    send(charToRaw("{\"valid\":false}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_VALIDATE_INCHIKEY_URL" = web$url())

test_that("post_validate_inchikey() returns a proper response.", {
  expect_type(
    post_validate_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N",
                           apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "logical"
  )
})

Sys.unsetenv("POST_VALIDATE_INCHIKEY_URL")

web$stop()