library(chemspiderapi)

context("get_queryId_status")

test_that("get_queryId_status() returns an NA_integer_ vector if query ID is wrong", {
  expect_error(
    get_queryId_status(queryId = "A wrong query ID",
                       count = FALSE, 
                       message = FALSE,
                       apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))
    )
})

test_that("get_queryId_status() returns an NA_character_ vector if apikey is wrong", {
  expect_error(
    get_queryId_status(queryId = "abcdefgh-abcd-abcd-abcd-abcdefghijkl",
                       count = FALSE, 
                       message = FALSE,
                       apikey = "A wrong apikey")
    )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/fe7fe60b-0b67-4b24-9d9b-1cf01b75f844/status", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"count\":2,\"message\":\"\",\"status\":\"Complete\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_QUERYID_URL" = web$url())

test_that("get_queryId_status() returns a proper response.", {
  expect_type(
    get_queryId_status(queryId = "fe7fe60b-0b67-4b24-9d9b-1cf01b75f844",
                       apikey = "abcdefghijklmnopqrstuvqxyz123456",
                       coerce = TRUE),
    "list"
  )
})

test_that("get_queryId_status() returns a proper response.", {
  expect_type(
    get_queryId_status(queryId = "fe7fe60b-0b67-4b24-9d9b-1cf01b75f844",
                       apikey = "abcdefghijklmnopqrstuvqxyz123456",
                       count = FALSE, message = FALSE, simplify = TRUE),
    "character"
  )
})

test_that("get_queryId_status() returns a proper response.", {
  expect_warning(
    get_queryId_status(queryId = "fe7fe60b-0b67-4b24-9d9b-1cf01b75f844",
                       apikey = "abcdefghijklmnopqrstuvqxyz123456",
                       simplify = TRUE)
  )
})

Sys.unsetenv("GET_QUERYID_URL")

web$stop()
