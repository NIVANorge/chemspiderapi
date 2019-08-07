## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE----------------------------------------------------------
#  install.packages(c("ratelimitr", "tidyverse"))

## ----setup---------------------------------------------------------------
## Loading chemspiderapi
library(chemspiderapi)

## Loading the demo data
data("demo_chemicals")

## Looking at the demo data
demo_chemicals

## ----eval=FALSE----------------------------------------------------------
#  ## Retrieving the API key from the keyring package.
#  ## Note that the keyring package is not loaded, but only the keyring::key_get() function is called.
#  apikey <- keyring::key_get(service = "ChemSpider API key", username = Sys.getenv("USERNAME"))

## ----eval=FALSE----------------------------------------------------------
#  ## The sleep_post_inchickey() function will automatically "sleep" for four seconds before returning its result.
#  ## This means it will run each query with a four second sleep in between, resulting in 15 queries per minute.
#  sleepy_post_inchikey <- function(...) {
#    result <- post_inchikey(...)
#    Sys.sleep(4)
#    result
#  }
#  
#  ## sapply() can be used in this context because the result of each iteration is a single vector.
#  demo_chemicals$queryId <- sapply(X = demo_chemicals$InChIKey, FUN = function(x) sleepy_post_inchikey(inchikey = x, apikey = apikey))
#  
#  demo_chemicals

## ----eval=FALSE----------------------------------------------------------
#  sleepy_get_queryId_status <- function(...) {
#    result <- get_queryId_status(...)
#    Sys.sleep(4)
#    result
#  }
#  
#  ## As the result of the query is an array, lapply() is used.
#  demo_chemicals$status <- lapply(X = demo_chemicals$queryId, FUN = function(x) sleepy_get_queryId_status(queryId = x, apikey = apikey))
#  
#  demo_chemicals

## ----eval=FALSE----------------------------------------------------------
#  sleepy_get_queryId_results <- function(...) {
#    result <- get_queryId_results(...)
#    Sys.sleep(4)
#    result
#  }
#  
#  ## mapply() is used because the input covers two columns
#  demo_chemicals$id <- mapply(FUN = function(x, y) sleepy_get_queryId_results(queryId = x, status = unlist(y)[1], apikey = apikey), x = demo_chemicals$queryId, y = demo_chemicals$status)
#  
#  demo_chemicals

## ----eval=FALSE----------------------------------------------------------
#  sleepy_get_recordId_details <- function(...) {
#    result <- get_recordId_details(...)
#    Sys.sleep(4)
#    result
#  }
#  
#  demo_chemicals$details <- lapply(X = demo_chemicals$id, FUN = function(x) sleepy_get_recordId_details(recordId = x, apikey = apikey, id = FALSE))
#  
#  demo_chemicals

## ------------------------------------------------------------------------
## Loading the ratelimitr package
library(ratelimitr)

## Re-setting the demo data
data("demo_chemicals")

## Note that the API key is already available from the previous example!

## ----eval=FALSE----------------------------------------------------------
#  ## In ratelimitr::limit_rate(), the ratelimitr::rate() function specifies the rate limit as calls per second.
#  limit_rate_post_inchikey <- limit_rate(post_inchikey, rate(n = 15, period = 60))
#  
#  ## sapply() can be used in this context because the result of each iteration is a single vector.
#  demo_chemicals$queryId <- sapply(X = demo_chemicals$InChIKey, FUN = function(x) limit_rate_post_inchikey(inchikey = x, apikey = apikey))
#  
#  demo_chemicals

## ----eval=FALSE----------------------------------------------------------
#  limit_rate_get_queryId_status <- limit_rate(get_queryId_status, rate(n = 15, period = 60))
#  
#  ## As the result of the query is an array, lapply() is used.
#  demo_chemicals$status <- lapply(X = demo_chemicals$queryId, FUN = function(x) limit_rate_get_queryId_status(queryId = x, apikey = apikey))
#  
#  demo_chemicals

## ----eval=FALSE----------------------------------------------------------
#  limit_rate_get_queryId_results <- limit_rate(get_queryId_results, rate(n = 15, period = 60))
#  
#  ## mapply() is used because the input covers two columns
#  demo_chemicals$id <- mapply(FUN = function(x, y) limit_rate_get_queryId_results(queryId = x, status = unlist(y)[1], apikey = apikey), x = demo_chemicals$queryId, y = demo_chemicals$status)
#  
#  demo_chemicals

## ----eval=FALSE----------------------------------------------------------
#  limit_rate_get_recordId_details <- limit_rate(get_recordId_details, rate(n = 15, period = 60))
#  
#  demo_chemicals$details <- lapply(X = demo_chemicals$id, FUN = function(x) limit_rate_get_recordId_details(recordId = x, apikey = apikey, id = FALSE))
#  
#  demo_chemicals

## ----eval=FALSE----------------------------------------------------------
#  ## Loading the tidyverse package family
#  library(tidyverse)
#  
#  ## Re-setting the demo data
#  data("demo_chemicals")
#  
#  ## Note that the API key is already available from the previous example!
#  
#  ## Using purrr::slowly() and purrr::rate_delay() to rate-limit the post_inchikey() function.
#  ## In this example, we introduce a pause of four seconds and use the verbose output (`quiet = FALSE`).
#  slowly_post_inchikey <- slowly(post_inchikey, rate = rate_delay(pause = 4), quiet = FALSE)

## ----eval=FALSE----------------------------------------------------------
#  tidyverse_demo <- demo_chemicals %>%
#    as_tibble() %>%
#    mutate(queryId = map_chr(InChIKey, ~ slowly_post_inchikey(inchikey = .x, apikey = apikey)))
#  
#  tidyverse_demo

## ----eval=FALSE----------------------------------------------------------
#  slowly_get_queryId_status <- slowly(get_queryId_status, rate = rate_delay(pause = 4), quiet = FALSE)
#  
#  tidyverse_demo <- tidyverse_demo %>%
#    mutate(status = map(queryId, ~ slowly_get_queryId_status(queryId = .x, apikey = apikey))) %>%
#    unnest(status)
#  
#  tidyverse_demo

## ----eval=FALSE----------------------------------------------------------
#  slowly_get_queryId_results <- slowly(get_queryId_results, rate = rate_delay(pause = 4), quiet = FALSE)
#  
#  tidyverse_demo <- tidyverse_demo %>%
#    mutate(id = map2_int(queryId, status, ~ slowly_get_queryId_results(queryId = .x, status = .y, apikey = apikey)))
#  
#  tidyverse_demo

## ----eval=FALSE----------------------------------------------------------
#  slowly_get_recordId_details <- slowly(get_recordId_details, rate = rate_delay(pause = 4), quiet = FALSE)
#  
#  tidyverse_demo <- tidyverse_demo %>%
#    mutate(details = map(id, ~ slowly_get_recordId_details(recordId = .x, apikey = apikey, id = FALSE))) %>%
#    unnest(details)
#  
#  tidyverse_demo

