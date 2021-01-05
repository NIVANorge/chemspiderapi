library(chemspiderapi)

context("get_mass_batch_queryId_results")

test_that("get_mass_batch_queryId_results() fails if no queryId is provided.", {
  expect_error(
    get_mass_batch_queryId_results()
  )
})

test_that("get_mass_batch_queryId_results() fails if a NULL queryId is provided.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = NULL)
  )
})

test_that("get_mass_batch_queryId_results() fails if a queryId is not a character vector.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = 123)
  )
})

test_that("get_mass_batch_queryId_results() fails if multiple queryIds are provided.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = c("0c98889f-5e8b-4974-aabb-31ab28c54261", "0c98889f-5e8b-4974-aabb-31ab28c54261"))
  )
})

test_that("get_mass_batch_queryId_results() fails if a queryId is too long.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261N")
  )
})

test_that("get_mass_batch_queryId_results() fails if a queryId is not hyphen divided into five parts.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabb31ab28c54261N")
  )
})

test_that("get_mass_batch_queryId_results() fails if the first part a queryId is of the wrong length.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889fN-5e8-4974-aabb-31ab28c54261")
  )
})

test_that("get_mass_batch_queryId_results() fails if the second part a queryId is of the wrong length.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8bN-4974-aab-31ab28c54261")
  )
})

test_that("get_mass_batch_queryId_results() fails if the third part a queryId is of the wrong length.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974N-aab-31ab28c54261")
  )
})

test_that("get_mass_batch_queryId_results() fails if the fourth part a queryId is of the wrong length.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabbN-31ab28c5426")
  )
})

test_that("get_mass_batch_queryId_results() fails if no status is provided.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261")
  )
})

test_that("get_mass_batch_queryId_results() fails if a NULL status is provided.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = NULL)
  )
})

test_that("get_mass_batch_queryId_results() fails if a multiple status are provided.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = c("Complete", "Complete"))
  )
})

test_that("get_mass_batch_queryId_results() fails if a non-character status is provided.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = 123)
  )
})

test_that("get_mass_batch_queryId_results() fails if the status is not complete.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Incomplete")
  )
})

test_that("get_mass_batch_queryId_results() fails if no API key is provided.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete")
  )
})

test_that("get_mass_batch_queryId_results() fails if NULL is provided as API key.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = NULL)
  )
})

test_that("get_mass_batch_queryId_results() fails if more than one API key is provided.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = c("API key one", "API key two"))
  )
})

test_that("get_mass_batch_queryId_results() fails if a numeric API key is provided.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = 1234567890)
  )
})

test_that("get_mass_batch_queryId_results() fails if a logical API key is provided.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = TRUE)
  )
})

test_that("get_mass_batch_queryId_results() fails if a non 32-character length API key is provided.", {
  expect_error(
    get_mass_batch_queryId_results(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/fe7fe60b-0b67-4b24-9d9b-1cf01b75f844/results", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"batchResults\":[{\"formula\":\"C9H8O4\",\"results\":[954,99021436]},{\"formula\":\"C17H21NO4\",\"results\":[2724,24503676,98641190]}]}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_MASS_BATCH_QUERYID_URL" = web$url())

test_that("get_mass_batch_queryId_results() returns a proper response.", {
  expect_type(
    get_mass_batch_queryId_results(queryId = "fe7fe60b-0b67-4b24-9d9b-1cf01b75f844",
                                   status = "Complete",
                                   apikey = "abcdefghijklmnopqrstuvqxyz123456",
                                   coerce = TRUE),
    "list"
  )
})

Sys.unsetenv("GET_MASS_BATCH_QUERYID_URL")

web$stop()
