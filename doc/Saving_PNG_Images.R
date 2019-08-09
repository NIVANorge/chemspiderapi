## ----eval=FALSE----------------------------------------------------------
#  install.packages("magick")

## ----eval=FALSE----------------------------------------------------------
#  library(chemspiderapi)
#  
#  apikey <- keyring::key_get(service = "ChemSpider API key", username = Sys.getenv("USERNAME"))

## ----eval=FALSE----------------------------------------------------------
#  caffeine_image_raw <- get_recordId_image(recordId = 2424L, apikey = apikey)

## ----eval=FALSE----------------------------------------------------------
#  library(magick)
#  
#  caffeine_png <- image_read(caffeine_image_raw)

## ----eval=FALSE----------------------------------------------------------
#  caffeine_png

## ----eval=FALSE----------------------------------------------------------
#  image_write(caffeine_png, path = "caffeine.png")

