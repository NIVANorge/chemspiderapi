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

# web <- setup({
#   app <- presser::new_app()
#   app$get("/hello/:user", function(req, res) {
#     res$send(paste0("Hello ", req$params$user, "!"))
#   })
#   presser::new_app_process(app)
# })
# teardown(web$stop())

