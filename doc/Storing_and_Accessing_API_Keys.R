## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  library("keyring")

## ----eval=FALSE----------------------------------------------------------
#  library(keyring)
#  
#  ## Note that Sys.getenv("USERNAME") is used to dynamically tie the API key to the current user.
#  key_set(service = "ChemSpider API key", username = Sys.getenv("USERNAME"))
#  

## ----eval=FALSE----------------------------------------------------------
#  apikey <- key_get(service = "ChemSpider API key", username = Sys.getenv("USERNAME"))

