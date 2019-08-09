## ----eval=FALSE----------------------------------------------------------
#  library(chemspiderapi)
#  
#  apikey <- keyring::key_get(service = "ChemSpider API key", username = Sys.getenv("USERNAME"))

## ----eval=FALSE----------------------------------------------------------
#  caffeine_mol <- get_recordId_mol(recordId = 2424L, apikey = apikey)
#  
#  caffeine_mol
#  
#  length(caffeine_mol)

## ----eval=FALSE----------------------------------------------------------
#  Sys.sleep(5)

## ----eval=FALSE----------------------------------------------------------
#  con <- file("caffeine.mol")
#  
#  writeLines(caffeine_mol, con = con)
#  
#  close(con)

## ----eval=FALSE----------------------------------------------------------
#  queryId <- post_mass(mass = 194, range = 0.002, apikey = apikey)

## ----eval=FALSE----------------------------------------------------------
#  status <- get_queryId_status(queryId = queryId, count = FALSE, message = FALSE, apikey = apikey)
#  
#  status

## ----eval=FALSE----------------------------------------------------------
#  caffeine_sdf <- get_queryId_results_sdf(queryId = queryId, status = status, apikey = apikey)
#  
#  head(caffeine_sdf)
#  
#  length(caffeine_sdf)

## ----eval=FALSE----------------------------------------------------------
#  con <- file("caffeine.sdf")
#  
#  writeLines(caffeine_sdf, con = con)
#  
#  close(con)

