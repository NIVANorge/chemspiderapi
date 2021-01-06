library(chemspiderapi)

context("get_queryId_results_sdf")

test_that("get_queryId_results_sdf() fails if no queryId is provided.", {
  expect_error(
    get_queryId_results_sdf()
  )
})

test_that("get_queryId_results_sdf() fails if a NULL queryId is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = NULL)
  )
})

test_that("get_queryId_results_sdf() fails if a queryId is not a character vector.", {
  expect_error(
    get_queryId_results_sdf(queryId = 123)
  )
})

test_that("get_queryId_results_sdf() fails if multiple queryIds are provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = c("0c98889f-5e8b-4974-aabb-31ab28c54261", "0c98889f-5e8b-4974-aabb-31ab28c54261"))
  )
})

test_that("get_queryId_results_sdf() fails if a queryId is too long.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261N")
  )
})

test_that("get_queryId_results_sdf() fails if a queryId is not hyphen divided into five parts.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb31ab28c54261N")
  )
})

test_that("get_queryId_results_sdf() fails if the first part a queryId is of the wrong length.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889fN-5e8-4974-aabb-31ab28c54261")
  )
})

test_that("get_queryId_results_sdf() fails if the second part a queryId is of the wrong length.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8bN-4974-aab-31ab28c54261")
  )
})

test_that("get_queryId_results_sdf() fails if the third part a queryId is of the wrong length.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974N-aab-31ab28c54261")
  )
})

test_that("get_queryId_results_sdf() fails if the fourth part a queryId is of the wrong length.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabbN-31ab28c5426")
  )
})

test_that("get_queryId_results_sdf() fails if no status is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261")
  )
})

test_that("get_queryId_results_sdf() fails if a NULL status is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = NULL)
  )
})

test_that("get_queryId_results_sdf() fails if a multiple status are provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = c("Complete", "Complete"))
  )
})

test_that("get_queryId_results_sdf() fails if a non-character status is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = 123)
  )
})

test_that("get_queryId_results_sdf() fails if the status is not complete.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Incomplete")
  )
})

test_that("get_queryId_results_sdf() fails if no API key is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete")
  )
})

test_that("get_queryId_results_sdf() fails if NULL is provided as API key.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = NULL)
  )
})

test_that("get_queryId_results_sdf() fails if more than one API key is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = c("API key one", "API key two"))
  )
})

test_that("get_queryId_results_sdf() fails if a numeric API key is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = 1234567890)
  )
})

test_that("get_queryId_results_sdf() fails if a logical API key is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = TRUE)
  )
})

test_that("get_queryId_results_sdf() fails if a non 32-character length API key is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = "abcdefghijklmnopqrstuvqxyz")
  )
})

test_that("get_queryId_results_sdf() fails if a wrong simplify is provided.", {
  expect_error(
    get_queryId_results_sdf(queryId = "0c98889f-5e8b-4974-aabb-31ab28c54261", status = "Complete", apikey = "abcdefghijklmnopqrstuvqxyz123456", simplify = "wrong")
  )
})

app <- webfakes::new_app()
app$use(webfakes::mw_json())
app$get("/2754e139-60e6-475b-9a0a-bf1591cb6a79/results/sdf", function(req, res) {
  res$
    set_status(200L)$
    send(charToRaw("{\"results\":\"H4sIAAAAAAAAC6WUW2+bMBSA3y3lP/hhDyDVxscGg6cVKTONiBZgC+tuL1NKmVKpaatcFE1T//sOabJEIdOmYFlgPg7fORwjCKV9m3ijyc1CKCnBgC+kTAih4DeTir9MoJ+kEIJQHILjChllslmFB8w2y3/OjQU4BEq/WLSW8jyL5EqE6mQt+TkWgbUY3fGN9h06w6K4H8ioa198roUOuu7R3tKlLwEPtYAXixARnGfR3ICKutayt3TpS8SFDv2ue7TvSxfL/ntR3BjlH1iK/7VgEZgdjigi1aaIMMcJqtsUA4M2xcDwFG3+P/KIYmB0isKJyjDQtKmhIFoGRACtWETQ7kND1bEho/QqT0hM37xf3WT1bUxASt0jvQYNH+x0GJPN6RJKz4JMIcwLrwLmM1BOwKQL0gHfBWAR0yxkIBzlGgbgTTUz6YXPglReAFOp2lqzQUzs918gn1M8hs95sbvxGbMb4FKbYIvKbDi6KjHe5o61rnUuC7eCCodTgWu3UclkOaHl42pe1QtU+OKw/nf1z5hkWfHl21s7sh+GH5MBu04Hg0G/+Fr2Wb6NteUwiYkPalfmuLToCnaVjOsf9bx+2GQIhfnz1PV4FJPpcvn02vPW6zWvpvVs8XR3W8959TjzLF7eVZN7Vi7nq2q5mte8ycGny9l943iFo0d+A5/iFp3PBgAA\"}"))
})

web <- webfakes::new_app_process(app)

Sys.setenv("GET_QUERYID_URL" = web$url())

test_that("get_queryId_results_sdf() returns a proper response.", {
  expect_type(
    get_queryId_results_sdf(queryId = "2754e139-60e6-475b-9a0a-bf1591cb6a79",
                            status = "Complete",
                            apikey = "abcdefghijklmnopqrstuvqxyz123456",
                            decode = TRUE, simplify = TRUE),
    "character"
  )
})

Sys.unsetenv("GET_QUERYID_URL")

web$stop()
