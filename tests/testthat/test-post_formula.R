library(chemspiderapi)

context("post_formula")

test_that("post_formula() fails if no formula is provided.", {
  expect_error(
    post_formula()
  )
})

test_that("post_formula() fails if NULL is provided as formula.", {
  expect_error(
    post_formula(formula = NULL)
  )
})

test_that("post_formula() fails if multiple formulas are provided.", {
  expect_error(
    post_formula(formula = c("C8H10N4O2", "C10H14BrNO2"))
  )
})

test_that("post_formula() fails if a non-character formula is provided.", {
  expect_error(
    post_formula(formula = 123)
  )
})

test_that("post_formula() fails if more than 20 data sources are provided.", {
  expect_error(
    post_formula(formula = "C8H10N4O2", 
                 dataSources = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v"))
  )
})

test_that("post_formula() fails if more than one orderBy is provided.", {
  expect_error(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                orderBy = c("recordid", "massdefect"), orderDirection = NULL)
  )
})

test_that("post_formula() fails if a false orderBy is provided.", {
  expect_error(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                orderBy = "thewrongthing", orderDirection = NULL)
  )
})

test_that("post_formula() fails if a non-character orderBy is provided.", {
  expect_error(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                orderBy = 123, orderDirection = NULL)
  )
})

test_that("post_formula() fails if more than one orderDirection is provided.", {
  expect_error(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                orderBy = NULL, orderDirection = c("ascending", "descending"))
  )
})

test_that("post_formula() fails if a non-character orderDirection is provided.", {
  expect_error(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                orderBy = NULL, orderDirection = 123)
  )
})

test_that("post_formula() fails if a false orderDirection is provided.", {
  expect_error(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                orderBy = NULL, orderDirection = "thewrongthing")
  )
})

test_that("post_formula() fails if no API key is provided.", {
  expect_error(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending")
  )
})

test_that("post_formula() fails if NULL is provided as API key.", {
  expect_error(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = NULL)
  )
})

test_that("post_formula() fails if more than one API key is provided.", {
  expect_error(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = c("API key one", "API key two"))
  )
})

test_that("post_formula() fails if a numeric API key is provided.", {
  expect_error(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = 1234567890)
  )
})

test_that("post_formula() fails if a logical API key is provided.", {
  expect_error(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = TRUE)
  )
})

test_that("post_formula() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = "abcdefghijklmnopqrstuvqxyz")
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

Sys.setenv("POST_FORMULA_URL" = web$url())

test_that("post_formula() returns a proper response.", {
  expect_type(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = "abcdefghijklmnopqrstuvqxyz123456",
                 coerce = TRUE),
    "list"
  )
})

test_that("post_formula() returns a proper response.", {
  expect_type(
    post_formula(formula = "C8H10N4O2", dataSources = NULL,
                 orderBy = "recordId", orderDirection = "ascending",
                 apikey = "abcdefghijklmnopqrstuvqxyz123456",
                 simplify = TRUE),
    "character"
  )
})

Sys.unsetenv("POST_FORMULA_URL")

web$stop()
