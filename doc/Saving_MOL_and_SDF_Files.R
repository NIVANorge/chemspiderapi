## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  library(chemspiderapi)
#  
#  apikey <- keyring::key_get(service = "ChemSpider API key", username = Sys.getenv("USERNAME"))

## ----eval=FALSE----------------------------------------------------------
#  caffeine_mol <- get_recordId_mol(recordId = 2424, apikey = apikey)

## ----eval=FALSE----------------------------------------------------------
#  ## Writing the MOL file. Note that the file is specified as a connection.
#  con <- file("caffeine.mol")
#  
#  writeLines(caffeine_mol, con = con)
#  
#  close(con)

## ----eval=FALSE----------------------------------------------------------
#  ## First the queryId is obtained
#  queryId <- post_mass(mass = 194, range = 0.002, apikey = apikey)
#  
#  ## Next, the status is controlled
#  status <- get_queryId_status(queryId = queryId, count = FALSE, message = FALSE, apikey = apikey)
#  
#  ## Note that per default, the MOL file is decompressed.
#  caffeine_sdf <- get_queryId_results_sdf(queryId = queryId, status = status, apikey = apikey)
#  
#  ## The file can now be written.
#  con <- file("caffeine.sdf")
#  
#  writeLines(caffeine_sdf, con = con)
#  
#  close(con)

