library(chemspiderapi)

context("post_name")

test_that("post_name() fails if no name is provided.", {
  expect_error(
    post_name()
  )
})

test_that("post_name() fails if NULL is provided as name.", {
  expect_error(
    post_name(name = NULL)
  )
})

test_that("post_name() fails if a non-character name is provided.", {
  expect_error(
    post_name(name = 123)
  )
})

test_that("post_name() fails if multiple names are provided.", {
  expect_error(
    post_name(name = c("caffeine", "2c-b"))
  )
})

test_that("post_name() fails if more than one orderBy is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = c("recordid", "massdefect"), orderDirection = NULL)
  )
})

test_that("post_name() fails if a false orderBy is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "thewrongthing", orderDirection = NULL)
  )
})

test_that("post_name() fails if a non-character orderBy is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = 123, orderDirection = NULL)
  )
})

test_that("post_name() fails if more than one orderDirection is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = NULL, orderDirection = c("ascending", "descending"))
  )
})

test_that("post_name() fails if a non-character orderDirection is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = NULL, orderDirection = 123)
  )
})

test_that("post_name() fails if a false orderDirection is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = NULL, orderDirection = "thewrongthing")
  )
})

test_that("post_name() fails if no API key is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "recordId", orderDirection = "ascending")
  )
})

test_that("post_name() fails if NULL is provided as API key.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "recordId", orderDirection = "ascending", apikey = NULL)
  )
})

test_that("post_name() fails if more than one API key is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "recordId", orderDirection = "ascending", apikey = c("API key one", "API key two"))
  )
})

test_that("post_name() fails if a numeric API key is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "recordId", orderDirection = "ascending", apikey = 1234567890)
  )
})

test_that("post_name() fails if a logical API key is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "recordId", orderDirection = "ascending", apikey = TRUE)
  )
})

test_that("post_name() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_name(name = "caffeine", orderBy = "recordId", orderDirection = "ascending", apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"queryId\":\"fe7fe60b-0b67-4b24-9d9b-1cf01b75f844\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_NAME_URL" = web$url())

test_that("post_name() returns a proper response.", {
  expect_type(
    post_name(name = "caffeine",
              apikey = "abcdefghijklmnopqrstuvqxyz123456",
              coerce = TRUE),
    "list"
  )
})

test_that("post_name() returns a proper response.", {
  expect_type(
    post_name(name = "caffeine",
              apikey = "abcdefghijklmnopqrstuvqxyz123456",
              simplify = TRUE),
    "character"
  )
})

Sys.unsetenv("POST_NAME_URL")

web$stop()
