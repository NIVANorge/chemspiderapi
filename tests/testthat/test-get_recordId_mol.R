library(chemspiderapi)

context("get_recordId_mol")

test_that("get_recordId_mol() fails if no recordId is provided.", {
  expect_error(
    get_recordId_mol()
  )
})

test_that("get_recordId_mol() fails if a NULL recordId is provided.", {
  expect_error(
    get_recordId_mol(recordId = NULL)
  )
})

test_that("get_recordId_mol() fails if a recordId is not a numeric vector.", {
  expect_error(
    get_recordId_mol(recordId = "recordId")
  )
})

test_that("get_recordId_mol() fails if multiple recordIds are provided.", {
  expect_error(
    get_recordId_mol(recordId = c("123", "456"))
  )
})


test_that("get_recordId_mol() fails if no API key is provided.", {
  expect_error(
    get_recordId_mol(recordId = 2424L)
  )
})

test_that("get_recordId_mol() fails if NULL is provided as API key.", {
  expect_error(
    get_recordId_mol(recordId = 2424L, apikey = NULL)
  )
})

test_that("get_recordId_mol() fails if more than one API key is provided.", {
  expect_error(
    get_recordId_mol(recordId = 2424L, apikey = c("API key one", "API key two"))
  )
})

test_that("get_recordId_mol() fails if a numeric API key is provided.", {
  expect_error(
    get_recordId_mol(recordId = 2424L, apikey = 1234567890)
  )
})

test_that("get_recordId_mol() fails if a logical API key is provided.", {
  expect_error(
    get_recordId_mol(recordId = 2424L, apikey = TRUE)
  )
})

test_that("get_recordId_mol() fails if a non 32-character length API key is provided.", {
  expect_error(
    get_recordId_mol(recordId = 2424L, apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/2424/mol", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"sdf\":\"\\n  ACD/Labs03221914022D\\n\\n 14 15  0  0  0  0  0  0  0  0  1 V2000\\n    5.1331   -0.3246    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\\n    4.7205   -1.5900    0.0000 N   0  0  0  0  0  0  0  0  0  0  0  0\\n    5.5017   -2.6628    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\\n    4.7205   -3.7357    0.0000 N   0  0  0  0  0  0  0  0  0  0  0  0\\n    3.4606   -3.3285    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\\n    3.4606   -1.9971    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\\n    2.3052   -1.3314    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\\n    1.1554   -1.9971    0.0000 N   0  0  0  0  0  0  0  0  0  0  0  0\\n    0.0000   -1.3314    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\\n    1.1554   -3.3285    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\\n    2.3052   -3.9942    0.0000 N   0  0  0  0  0  0  0  0  0  0  0  0\\n    2.3052   -5.3257    0.0000 C   0  0  0  0  0  0  0  0  0  0  0  0\\n    0.0000   -3.9942    0.0000 O   0  0  0  0  0  0  0  0  0  0  0  0\\n    2.3052    0.0000    0.0000 O   0  0  0  0  0  0  0  0  0  0  0  0\\n  1  2  1  0  0  0  0\\n  2  3  1  0  0  0  0\\n  2  6  1  0  0  0  0\\n  3  4  2  0  0  0  0\\n  4  5  1  0  0  0  0\\n  5  6  2  0  0  0  0\\n  5 11  1  0  0  0  0\\n  6  7  1  0  0  0  0\\n  7  8  1  0  0  0  0\\n  7 14  2  0  0  0  0\\n  8  9  1  0  0  0  0\\n  8 10  1  0  0  0  0\\n 10 11  1  0  0  0  0\\n 10 13  2  0  0  0  0\\n 11 12  1  0  0  0  0\\nM  END\\n\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_RECORDID_URL" = web$url())

test_that("get_recordId_mol() returns a proper response.", {
  expect_type(
    get_recordId_mol(recordId = 2424L,
                     apikey = "abcdefghijklmnopqrstuvqxyz123456",
                     simplify = TRUE),
    "character"
  )
})

Sys.unsetenv("GET_RECORDID_URL")

web$stop()
