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
