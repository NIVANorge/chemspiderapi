library(chemspiderapi)

context("post_smiles")

test_that("post_smiles() fails if no smiles is provided.", {
  expect_error(
    post_smiles()
  )
})

test_that("post_smiles() fails if a NULL smiles is provided.", {
  expect_error(
    post_smiles(smiles = NULL)
  )
})

test_that("post_smiles() fails if smiles is not a character vector.", {
  expect_error(
    post_smiles(smiles = 123)
  )
})

test_that("post_smiles() fails if an InChI string is provided.", {
  expect_error(
    post_smiles(smiles = "InChI=1S/C8H10N4O2/c1-10-4-9-6-5(10)7(13)12(3)8(14)11(6)2/h4H,1-3H3")
  )
})

test_that("post_smiles() fails if multiple smiles are provided", {
  expect_error(
    post_smiles(smiles = c("CN1C=NC2=C1C(=O)N(C(=O)N2C)C", "COc1cc(CCN)c(OC)cc1Br"))
  )
})

test_that("post_smiles() fails if no API key is provided.", {
  expect_error(
    post_smiles(smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C")
  )
})

test_that("post_smiles() fails if NULL is provided as API key.", {
  expect_error(
    post_smiles(smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C", apikey = NULL)
  )
})

test_that("post_smiles() fails if more than one API key is provided.", {
  expect_error(
    post_smiles(smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C", apikey = c("API key one", "API key two"))
  )
})

test_that("post_smiles() fails if a numeric API key is provided.", {
  expect_error(
    post_smiles(smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C", apikey = 1234567890)
  )
})

test_that("post_smiles() fails if a logical API key is provided.", {
  expect_error(
    post_smiles(smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C", apikey = TRUE)
  )
})

test_that("post_smiles() fails if a non 32-character length API key is provided.", {
  expect_error(
    post_smiles(smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C", apikey = "abcdefghijklmnopqrstuvqxyz")
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

Sys.setenv("POST_SMILES_URL" = web$url())

test_that("post_smiles() returns a proper response.", {
  expect_type(
    post_smiles(smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C",
                apikey = "abcdefghijklmnopqrstuvqxyz123456",
                coerce = TRUE),
    "list"
  )
})

test_that("post_smiles() returns a proper response.", {
  expect_type(
    post_smiles(smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C",
                apikey = "abcdefghijklmnopqrstuvqxyz123456",
                simplify = TRUE),
    "character"
  )
})

Sys.unsetenv("POST_SMILES_URL")

web$stop()
