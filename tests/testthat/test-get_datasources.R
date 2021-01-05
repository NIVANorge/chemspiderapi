library(chemspiderapi)

context("get_datasources")

test_that("get_datasources() returns nothing if API key length is wrong.", {
  expect_error(
    get_datasources(apikey = "A wrong API key")
    )
})

test_that("get_datasources() returns nothing if API key type is wrong.", {
  expect_error(
    get_datasources(apikey = 1234567890L)
    )
})

test_that("get_datasources() returns nothing if multiple API keys are provided.", {
  expect_error(
    get_datasources(apikey = c("A wrong API key", "Another wrong API key"))
    )
})

test_that("get_datasources() stops if coerce type is wrong.", {
  expect_error(
    get_datasources(coerce = 1234567890L)
  )
})

test_that("get_datasources() stops if multiple coerce values are provided.", {
  expect_error(
    get_datasources(coerce = c(TRUE, FALSE))
  )
})

test_that("get_datasources() returns nothing if simplify type is wrong.", {
  expect_error(
    get_datasources(simplify = 1234567890L)
  )
})

test_that("get_datasources() returns nothing if multiple simplify values are provided.", {
  expect_error(
    get_datasources(simplify = c(TRUE, FALSE))
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"dataSources\":[\"a\",\"b\"]}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_DATASOURCES_URL" = web$url())

test_that("get_datasources() returns a proper response.", {
  expect_type(
    get_datasources(apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
})

test_that("get_datasources() returns a proper response.", {
  expect_type(
    get_datasources(apikey = "abcdefghijklmnopqrstuvqxyz123456", coerce = TRUE),
    "list"
  )
})

test_that("get_datasources() returns a proper response.", {
  expect_type(
    get_datasources(apikey = "abcdefghijklmnopqrstuvqxyz123456", simplify = TRUE),
    "character"
  )
})

Sys.unsetenv("GET_DATASOURCES_URL")

web$stop()
