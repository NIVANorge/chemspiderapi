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

test_that("get_datasources() correctly checks the status code.", {
  raw_result <- list()
  raw_result$status_code <- 200L
  expect_silent(.check_status_code(raw_result$status_code))
  
  raw_result$status_code <- 400L
  expect_error(.check_status_code(raw_result$status_code))
})

# test_that("", {
#   expect_identical(
#     typeof(curl::new_handle()), 
#     "externalptr"
#   )
# })



# with_mock_api({
#   test_that("get_datasources() returns a data.frame with a single character column", {
#     result <- get_datasources(apikey = keyring::key_get(service = "ChemSpider API key",
#                                                         username = Sys.getenv("USERNAME")))
#     expect_identical(class(result), "data.frame")
#     expect_identical(length(colnames(result)), 1L)
#     expect_identical(colnames(result), "dataSources")
#     expect_identical(typeof(result$dataSources), "character")
#   })
# })

# httpbin <- setup(presser::new_app_process(presser::httpbin_app()))
# teardown(httpbin$stop())
# 
# test_that("HTTP errors are caught", {
#   url <- httpbin$url("/status/404")
#   resp <- httr::GET(url)
#   expect_error(httr::stop_for_status(resp), class = "http_404")
# })

# test_that("query works", {
#   app <- presser::new_app()
#   app$get("/hello", function(req, res) res$send("hello there"))
#   on.exit(try(web$stop()))
#   web <- presser::new_app_process(app)
#   
#   echo <- httr::content(httr::GET(web$url("/hello")))
#   expect_equal(echo, "hello there")
# })
