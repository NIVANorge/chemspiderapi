library(chemspiderapi)

context("post_convert")

test_that("post_convert() returns an NA vector if input is wrong", {
  expect_error(suppressWarnings(post_convert(input = "Not an InChIKey",
                                             inputFormat = "InChIKey",
                                             outputFormat = "SMILES",
                                             apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))))
})

test_that("post_convert() returns an NA_character_ vector if input format is wrong", {
  expect_error(suppressWarnings(post_convert(input = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N",
                                             inputFormat = "Not an input format",
                                             outputFormat = "SMILES",
                                             apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))))
})

test_that("post_convert() returns an NA_character_ vector if output format is wrong", {
  expect_error(suppressWarnings(post_convert(input = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N",
                                             inputFormat = "InChIKey",
                                             outputFormat = "Not an output format",
                                             apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))))
})

test_that("post_convert() returns an NA_character_ vector if apikey is wrong", {
  expect_error(suppressWarnings(post_convert(input = "BSYNRYMUTXBXSQ-UHFFFAOYSA-N",
                                             inputFormat = "InChIKey",
                                             outputFormat = "SMILES",
                                             apikey = "A wrong apikey")))
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$post("/", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"output\":\"Cn1cnc2c1c(=O)n(C)c(=O)n2C\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("POST_CONVERT_URL" = web$url())

test_that("post_convert() returns a proper response.", {
  expect_type(
    post_convert(input = "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3",
                 inputFormat = "InChI",
                 outputFormat = "SMILES",
                 apikey = "abcdefghijklmnopqrstuvqxyz123456",
                 coerce = TRUE),
    "list"
  )
})

test_that("post_convert() returns a proper response.", {
  expect_type(
    post_convert(input = "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3",
                 inputFormat = "InChI",
                 outputFormat = "SMILES",
                 apikey = "abcdefghijklmnopqrstuvqxyz123456",
                 simplify = TRUE),
    "character"
  )
})

Sys.unsetenv("POST_CONVERT_URL")

web$stop()
