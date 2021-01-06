library(chemspiderapi)

context("get_recordId_externalreferences")
test_that("get_recordId_externalreferences() fails if no recordId is provided.", {
  expect_error(
    get_recordId_externalreferences()
  )
})

test_that("get_recordId_externalreferences() fails if a NULL recordId is provided.", {
  expect_error(
    get_recordId_externalreferences(recordId = NULL)
  )
})

test_that("get_recordId_externalreferences() fails if a recordId is not a numeric vector.", {
  expect_error(
    get_recordId_externalreferences(recordId = "recordId")
  )
})

test_that("get_recordId_externalreferences() fails if multiple recordIds are provided.", {
  expect_error(
    get_recordId_externalreferences(recordId = c("123", "456"))
  )
})

test_that("get_recordId_externalreferences() fails if more than 20 data sources are provided.", {
  expect_error(
    get_recordId_externalreferences(recordId = 2424L, dataSources = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v"))
  )
})

test_that("get_recordId_externalreferences() fails if no API key is provided.", {
  expect_error(
    get_recordId_externalreferences(recordId = 2424L, dataSources = NULL)
  )
})

test_that("get_recordId_externalreferences() fails if NULL is provided as API key.", {
  expect_error(
    get_recordId_externalreferences(recordId = 2424L, dataSources = NULL, apikey = NULL)
  )
})

test_that("get_recordId_externalreferences() fails if more than one API key is provided.", {
  expect_error(
    get_recordId_externalreferences(recordId = 2424L, dataSources = NULL, apikey = c("API key one", "API key two"))
  )
})

test_that("get_recordId_externalreferences() fails if a numeric API key is provided.", {
  expect_error(
    get_recordId_externalreferences(recordId = 2424L, dataSources = NULL, apikey = 1234567890)
  )
})

test_that("check_apikey() fails if a logical API key is provided.", {
  expect_error(
    check_apikey(recordId = 2424L, dataSources = NULL, apikey = TRUE)
  )
})

test_that("get_recordId_externalreferences() fails if a non 32-character length API key is provided.", {
  expect_error(
    get_recordId_externalreferences(recordId = 2424L, dataSources = NULL, apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})

test_that("get_recordId_externalreferences() fails if a wrong coerce is provided is provided.", {
  expect_error(
    get_recordId_externalreferences(recordId = 2424L, 
                                    apikey = "abcdefghijklmnopqrstuvqxyz123456",
                                    coerce = "wrong")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/2424/externalreferences", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"externalReferences\":[{\"source\":\"1717 CheMall\",\"sourceUrl\":\"http://www.1717chem.com\",\"externalId\":\"BT000262\",\"externalUrl\":\"http://3bsccorp.com/botanic-extracts-enzymes-and-coenzymes/BT000262\"}]}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_RECORDID_URL" = web$url())

test_that("get_recordId_externalreferences() returns a proper response.", {
  expect_type(
    get_recordId_externalreferences(recordId = 2424L,
                                    apikey = "abcdefghijklmnopqrstuvqxyz123456",
                                    coerce = TRUE),
    "list"
  )
})

Sys.unsetenv("GET_RECORDID_URL")

web$stop()

# app <- webfakes::new_app()
# app$use(webfakes::mw_json())
# app$get("/2424/externalreferences?dataSources=PubChem", function(req, res) {
#   res$
#     set_status(200L)$
#     send(charToRaw("{\"externalReferences\":[{\"source\":\"1717 CheMall\",\"sourceUrl\":\"http://www.1717chem.com\",\"externalId\":\"BT000262\",\"externalUrl\":\"http://3bsccorp.com/botanic-extracts-enzymes-and-coenzymes/BT000262\"}]}"))
# })
# 
# web <- webfakes::new_app_process(app)
# 
# Sys.setenv("GET_RECORDID_URL" = web$url())
# 
# test_that("get_recordId_externalreferences() returns a proper response.", {
#   expect_type(
#     get_recordId_externalreferences(recordId = 2424L,
#                                     dataSources = "PubChem",
#                                     apikey = "abcdefghijklmnopqrstuvqxyz123456",
#                                     coerce = TRUE),
#     "list"
#   )
# })
# 
# Sys.unsetenv("GET_RECORDID_URL")
# 
# web$stop()
