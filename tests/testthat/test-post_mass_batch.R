library(chemspiderapi)

context("post_mass_batch")

test_that("post_mass_batch() fails if non-numeric mass is provided.", {
  expect_error(
    post_mass_batch(mass = "hundredfifty", range = 0.002)
  )
})

test_that("post_mass_batch() fails if non-numeric range is provided.", {
  expect_error(
    post_mass_batch(mass = 150, range = "naughtpointzerozerotwo")
  )
})

test_that("post_mass_batch() fails if mass is too small.", {
  expect_error(
    post_mass_batch(mass = 0.1, range = 0.002)
  )
})

test_that("post_mass_batch() fails if mass is too large.", {
  expect_error(
    post_mass_batch(mass = 11100, range = 0.002)
  )
})

test_that("post_mass_batch() fails if range is too small.", {
  expect_error(
    post_mass_batch(mass = 150, range = 0.00001)
  )
})

test_that("post_mass_batch() fails if range is too large.", {
  expect_error(
    post_mass_batch(mass = 150, range = 1000)
  )
})

test_that("post_mass_batch() fails if mass and range do not have the same length.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001))
  )
})

test_that("post_mass_batch() fails if more than 20 data sources are provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), dataSources = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v"))
  )
})

test_that("post_mass_batch() fails if more than one orderBy is provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), dataSources = NULL, orderBy = c("recordid", "massdefect"), orderDirection = NULL)
  )
})


test_that("post_mass_batch() fails if a false orderBy is provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), dataSources = NULL, orderBy = "thewrongthing", orderDirection = NULL)
  )
})

test_that("post_mass_batch() fails if a non-character orderBy is provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), dataSources = NULL, orderBy = 123, orderDirection = NULL)
  )
})

test_that("post_mass_batch() fails if more than one orderDirection is provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), dataSources = NULL, orderBy = NULL, orderDirection = c("ascending", "descending"))
  )
})

test_that("post_mass_batch() fails if a non-character orderDirection is provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), dataSources = NULL, orderBy = NULL, orderDirection = 123)
  )
})

test_that("post_mass_batch() fails if a false orderDirection is provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), dataSources = NULL, orderBy = NULL, orderDirection = "thewrongthing")
  )
})







test_that("post_mass_batch() fails if no API key is provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), dataSources = NULL, orderBy = "recordId", orderDirection = "descending")
  )
})

test_that("post_mass_batch() fails if NULL is provided as API key.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), dataSources = NULL, orderBy = "recordId", orderDirection = "descending", apikey = NULL)
  )
})

test_that("post_mass_batch() fails if more than one API key is provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), dataSources = NULL, orderBy = "recordId", orderDirection = "descending", apikey = c("API key one", "API key two"))
  )
})

test_that("post_mass_batch() fails if a numeric API key is provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), dataSources = NULL, orderBy = "recordId", orderDirection = "descending", apikey = 1234567890)
  )
})

test_that("post_mass_batch() fails if a logical API key is provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), dataSources = NULL, orderBy = "recordId", orderDirection = "descending", apikey = TRUE)
  )
})

test_that("post_mass_batch() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002), dataSources = NULL, orderBy = "recordId", orderDirection = "descending", apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})

test_that("post_mass_batch() fails if a wrong coerce is provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002),
                    apikey = "abcdefghijklmnopqrstuvqxyz123456", coerce = "wrong")
  )
})

test_that("post_mass() fails if a wrong simplify is provided.", {
  expect_error(
    post_mass_batch(mass = c(150, 140, 120), range = c(0.002, 0.001, 0.002),
                    apikey = "abcdefghijklmnopqrstuvqxyz123456", simplify = "wrong")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"queryId\":\"e95e0aea-ee69-4590-8e94-5addc43a3876\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_MASS_BATCH_URL" = web$url())

test_that("post_mass_batch() returns a proper response.", {
  expect_type(
    post_mass_batch(mass = c(150, 140, 120), 
                    range = c(0.002, 0.001, 0.002),
                    apikey = "abcdefghijklmnopqrstuvqxyz123456",
                    coerce = TRUE, simplify = TRUE),
    "character"
  )
})

test_that("post_mass_batch() returns a proper response.", {
  expect_type(
    post_mass_batch(mass = c(150, 140, 120), 
                    range = c(0.002, 0.001, 0.002),
                    dataSources = "PubMed",
                    apikey = "abcdefghijklmnopqrstuvqxyz123456",
                    coerce = TRUE, simplify = TRUE),
    "character"
  )
})

Sys.unsetenv("POST_MASS_BATCH_URL")

web$stop()
