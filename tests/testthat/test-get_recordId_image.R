library(chemspiderapi)

context("get_recordId_image")

test_that("get_recordId_image() fails if no recordId is provided.", {
  expect_error(
    get_recordId_image()
  )
})

test_that("get_recordId_image() fails if a NULL recordId is provided.", {
  expect_error(
    get_recordId_image(recordId = NULL)
  )
})

test_that("get_recordId_image() fails if a recordId is not a numeric vector.", {
  expect_error(
    get_recordId_image(recordId = "recordId")
  )
})

test_that("get_recordId_image() fails if multiple recordIds are provided.", {
  expect_error(
    get_recordId_image(recordId = c("123", "456"))
  )
})

test_that("get_recordId_image() fails if no API key is provided.", {
  expect_error(
    get_recordId_image(recordId = 2424L)
  )
})

test_that("get_recordId_image() fails if NULL is provided as API key.", {
  expect_error(
    get_recordId_image(recordId = 2424L, apikey = NULL)
  )
})

test_that("get_recordId_image() fails if more than one API key is provided.", {
  expect_error(
    get_recordId_image(recordId = 2424L, apikey = c("API key one", "API key two"))
  )
})

test_that("get_recordId_image() fails if a numeric API key is provided.", {
  expect_error(
    get_recordId_image(recordId = 2424L, apikey = 1234567890)
  )
})

test_that("get_recordId_image() fails if a logical API key is provided.", {
  expect_error(
    get_recordId_image(recordId = 2424L, apikey = TRUE)
  )
})

test_that("get_recordId_image() fails if a non 32-character length API key is provided.", {
  expect_error(
    get_recordId_image(recordId = 2424L, apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})

test_that("get_recordId_image() fails if a non 32-character length API key is provided.", {
  expect_error(
    get_recordId_image(recordId = 2424L, apikey = "abcdefghijklmnopqrstuvqxyz123456", simplify = "wrong")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/2424/image", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"image\":\"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\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_RECORDID_URL" = web$url())

test_that("get_recordId_image() returns a proper response.", {
  expect_type(
    get_recordId_image(recordId = 2424L,
                       apikey = "abcdefghijklmnopqrstuvqxyz123456",
                       decode = TRUE),
    "list"
  )
})

test_that("get_recordId_image() returns a proper response.", {
  expect_type(
    get_recordId_image(recordId = 2424L,
                       apikey = "abcdefghijklmnopqrstuvqxyz123456",
                       simplify = TRUE),
    "character"
  )
})

Sys.unsetenv("GET_RECORDID_URL")

web$stop()
