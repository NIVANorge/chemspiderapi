library(chemspiderapi)

context("get_queryId_results")

test_that("get_queryId_results() fails if query ID is wrong", {
  expect_error(
    get_queryId_results(queryId = "A wrong query ID",
                        status = "Complete",
                        apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))
    )
})

test_that("get_queryId_results() fails if apikey is wrong", {
  expect_error(
    get_queryId_results(queryId = "abcdefgh-abcd-abcd-abcd-abcdefghijkl",
                        status = "Complete",
                        apikey = "A wrong apikey")
    )
})

test_that("get_queryId_results() returns an NA_integer_ vector if status is Incomplete", {
  expect_error(
    get_queryId_results(queryId = "abcdefgh-abcd-abcd-abcd-abcdefghijkl",
                        status = "Incomplete",
                        apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))
    )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/fe7fe60b-0b67-4b24-9d9b-1cf01b75f844/results", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"results\":[{\"formula\":\"C9H8O4\",\"results\":[954,99021436]}]}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_QUERYID_URL" = web$url())

test_that("get_queryId_results() returns a proper response.", {
  expect_type(
    get_queryId_results(queryId = "fe7fe60b-0b67-4b24-9d9b-1cf01b75f844",
                        status = "Complete",
                        apikey = "abcdefghijklmnopqrstuvqxyz123456",
                        coerce = TRUE),
    "list"
  )
})

Sys.unsetenv("GET_QUERYID_URL")

web$stop()
