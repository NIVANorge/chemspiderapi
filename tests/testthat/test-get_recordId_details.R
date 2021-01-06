library(chemspiderapi)

context("get_recordId_details")

test_that("get_recordId_details() returns an NA vector if record ID is wrong", {
  expect_error(
    get_recordId_details(recordId = "A wrong query ID",
                         apikey = keyring::key_get("R Keyring Service", "ChemSpider API Key"))
    ) 
})

test_that("get_recordId_details() returns an NA vector if apikey is wrong", {
  expect_error(
    get_recordId_details(recordId = 2157L, 
                         apikey = "A wrong apikey")
    )
})

test_that("get_recordId_details() fails if coerce is wrong.", {
  expect_error(
    get_recordId_details(recordId = 2157L, 
                         apikey = "abcdefghijklmnopqrstuvqxyz123456",
                         coerce = "wrong")
  )
})

# app <- webfakes::new_app()
# app$use(webfakes::mw_json())
# app$get("/2424/details?fields=SMILES,Formula", function(req, res) {
#   res$
#     set_status(200L)$
#     send(charToRaw("{\"id\":2424,\"smiles\":\"Cn1cnc2c1c(=O)n(c(=O)n2C)C\",\"formula\":\"C_{8}H_{10}N_{4}O_{2}\"}"))
# })
# 
# web <- webfakes::new_app_process(app)
# 
# Sys.setenv("GET_RECORDID_URL" = web$url())
# 
# test_that("get_recordId_details() returns a proper response.", {
#   expect_type(
#     get_recordId_details(recordId = 2424L,
#                          fields = c("SMILES", "Formula"),
#                          apikey = "abcdefghijklmnopqrstuvqxyz123456",
#                          id = FALSE, simplify_formula = TRUE,
#                          coerce = TRUE),
#     "list"
#   )
# })
# 
# Sys.unsetenv("GET_RECORDID_URL")
# 
# web$stop()
