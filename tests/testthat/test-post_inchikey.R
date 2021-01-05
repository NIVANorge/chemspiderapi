library(chemspiderapi)

context("post_inchikey")

test_that("post_inchikey() fails pre-query if InChIKey is wrong", {
  expect_error(
    post_inchikey(inchikey = " A wrong InChIKey", 
                  apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))
    )
})

test_that("post_inchikey() fails pre-query if apikey is wrong", {
  expect_error(
    post_inchikey(inchikey = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N", 
                  apikey = "A wrong apikey")
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

Sys.setenv("POST_INCHIKEY_URL" = web$url())

test_that("post_inchikey() returns a proper response.", {
  expect_type(
    post_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N",
                  apikey = "abcdefghijklmnopqrstuvqxyz123456",
                  coerce = TRUE),
    "list"
  )
})

test_that("post_inchikey() returns a proper response.", {
  expect_type(
    post_inchikey(inchikey = "RYYVLZVUVIJVGH-UHFFFAOYSA-N",
                  apikey = "abcdefghijklmnopqrstuvqxyz123456",
                  simplify = TRUE),
    "character"
  )
})

Sys.unsetenv("POST_INCHIKEY_URL")

web$stop()
