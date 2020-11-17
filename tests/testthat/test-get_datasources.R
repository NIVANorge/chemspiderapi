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

test_that("get_datasources() uses the correct header.", {
  header <- list("Content-Type" = "", "apikey" = paste0(sample(letters, size = 32, replace = TRUE), collapse = ""))
  expect_length(header, 2L)
  expect_type(header, "list")
})

test_that("get_datasources() uses the correct base URL.", {
  url <- "https://api.rsc.org/compounds/v1/lookups/datasources"
  expect_identical(url, "https://api.rsc.org/compounds/v1/lookups/datasources")
  expect_type(url, "character")
})

test_that("get_datasources() uses the correct cURL handle.", {
  header <- list("Content-Type" = "", "apikey" = paste0(sample(letters, size = 32, replace = TRUE), collapse = ""))
  handle <- curl::new_handle()
  curl::handle_setopt(handle, customrequest = "GET")
  curl::handle_setheaders(handle, .list = header)
  expect_type(handle, "externalptr")
  expect_length(curl:::handle_getheaders(handle), 3L)
})

test_that("get_datasources() correctly checks the status code.", {
  raw_result <- list()
  raw_result$status_code <- 200L
  expect_silent(.check_status_code(raw_result$status_code))
  
  raw_result$status_code <- 400L
  expect_error(.check_status_code(raw_result$status_code))
})

web <- setup({
  app <- presser::new_app()
  app$use(presser::mw_json())
  app$get("/", function(req, res) {
    res$
      set_status(200L)$
      send(charToRaw("{\"dataSources\":[\"a\",\"b\"]}"))
  })
  presser::new_app_process(app)
})

test_that("get_datasources() returns a proper response.", {
  header <- list("Content-Type" = "", "apikey" = "apikey")
  handle <- curl::new_handle()
  curl::handle_setopt(handle, customrequest = "GET")
  curl::handle_setheaders(handle, .list = header)
  url <- web$url()
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  expect_type(raw_result, "list")
  expect_length(raw_result, 7L)
  expect_identical(raw_result$status_code, 200L)
  expect_type(raw_result$content, "raw")
  
})

teardown(web$stop())
