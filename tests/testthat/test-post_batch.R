library(chemspiderapi)

context("post_batch")

test_that("post_batch() fails if no recordIds are provided.", {
  expect_error(
    post_batch(fields = "all", apikey = "apikey")
  )
})

test_that("post_batch() fails if NULL recordIds are provided.", {
  expect_error(
    post_batch(recordIds = NULL, fields = "all", apikey = "apikey")
  )
})

test_that("post_batch() fails if recordIds are not a numeric vector.", {
  expect_error(
    post_batch(recordIds = c("recordId1", "recordId2"), fields = "all", apikey = "apikey")
  )
})

test_that("post_batch() fails if only a single recordId is provided.", {
  expect_error(
    post_batch(recordIds = 2424L, fields = "all", apikey = "apikey")
  )
})

test_that("post_batch() fails if more than 100 recordIds are provided.", {
  expect_error(
    post_batch(recordIds = 1:101, fields = "all", apikey = "apikey")
  )
})

test_that("post_batch() fails if no character input is provided.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = 1)
  )
})

test_that("post_batch() fails if the wrong character string is provided as input.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "Something")
  )
})

test_that("post_batch() fails if no API key is provided.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "all")
  )
})

test_that("post_batch() fails if NULL is provided as API key.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "all", apikey = NULL)
  )
})

test_that("post_batch() fails if more than one API key is provided.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "all", apikey = c("API key one", "API key two"))
  )
})

test_that("post_batch() fails if a numeric API key is provided.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "all", apikey = 1234567890)
  )
})

test_that("post_batch() fails if a logical API key is provided.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "all", apikey = TRUE)
  )
})

test_that("post_batch() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "all", apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})

test_that("post_batch() fails if \"coerce\" is not logical.", {
  expect_error(
    post_batch(recordIds = c(2424L, 2345L), fields = "all", apikey = "abcdefghijklmnopqrstuvqxyz123456", coerce = "what")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"records\":[{\"id\":2345,\"formula\":\"C_{20}H_{24}BrN_{4}O_{6}\",\"commonName\":\"2-(3-{2-Bromo-5-[(2,4-diaminopyrimidin-1-ium-5-yl)methyl]-3-methoxyphenoxy}propyl)pentanedioate\"},{\"id\":2424,\"formula\":\"C_{8}H_{10}N_{4}O_{2}\",\"commonName\":\"Caffeine\"}]}"))
})

web <- webfakes::new_app_process(app, start = TRUE)

Sys.setenv("POST_BATCH_URL" = web$url())

test_that("get_datasources() returns a proper response.", {
  expect_type(
    chemspiderapi::post_batch(recordIds = c(2424L, 2345L), apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
})

test_that("get_datasources() returns a proper response.", {
  expect_type(
    chemspiderapi::post_batch(recordIds = c(2424L, 2345L), fields = c("CommonName", "SMILES"), apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
})

test_that("get_datasources() returns a proper response.", {
  expect_type(
    chemspiderapi::post_batch(recordIds = c(2424L, 2345L), fields = "CommonName", apikey = "abcdefghijklmnopqrstuvqxyz123456"),
    "list"
  )
})

test_that("get_datasources() returns a proper response.", {
  expect_type(
    chemspiderapi::post_batch(recordIds = c(2424L, 2345L), apikey = "abcdefghijklmnopqrstuvqxyz123456", id = FALSE, simplify_formula = TRUE, coerce = TRUE),
    "list"
  )
})

Sys.unsetenv("POST_BATCH_URL")

web$stop()
