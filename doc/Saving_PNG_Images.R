## ----include=FALSE-------------------------------------------------------
library(httptest)
start_vignette("png")

## ----eval=FALSE----------------------------------------------------------
#  install.packages("magick")

## ------------------------------------------------------------------------
library(chemspiderapi)

apikey <- keyring::key_get(service = "ChemSpider API key", username = Sys.getenv("USERNAME"))

## ------------------------------------------------------------------------
caffeine_image_raw <- get_recordId_image(recordId = 2424L, apikey = apikey)

## ------------------------------------------------------------------------
library(magick)

caffeine_png <- image_read(caffeine_image_raw)

## ------------------------------------------------------------------------
caffeine_png

## ----eval=FALSE----------------------------------------------------------
#  image_write(caffeine_png, path = "caffeine.png")

## ----include=FALSE-------------------------------------------------------
end_vignette()

