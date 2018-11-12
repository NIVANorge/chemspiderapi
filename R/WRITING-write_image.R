#' Write a .png image file
#' 
#' Convenience function to write a .png image file, e.g., after downloading with \code{chemspiderapi::get_recordId_image()}.
#' 
#' A convenience function to write downloaded .png images, as obtained from \code{chemspiderapi::get_recordId_image()}.\cr
#' \cr
#' If successful, it writes a 150 x 150 px .png image (as returned by ChemSpider).\cr
#' \cr
#' If not successful, returns \code{NA} without writing an image file.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
#' 
#' @param image Either a base64-encoded character string or a numeric (double) array of the .png image.
#' @param file A character string indicating where and under which name to save the image.
#' @return writes a .png image file
#' @examples 
#' \dontrun{
#' ## Saving the .png file for aspirin
#' recordId <- 2157L
#' apikey <- "a valid 32-character ChemSpider apikey"
#' image <- get_recordId_image(recordId = recordId, apikey = apikey)
#' write_image(image = image, file = "aspirin.png")
#' }
#' @export
write_image <- function(image, file) {
  
  if (length(image) > 1) {
    warning("This function can only handle a single \"image\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA)
  }
  
  if (typeof(image) == "character") {
    image <- jsonlite::base64_dec(image)
    image <- png::readPNG(image)
  }
  
  png::writePNG(image = image, target = file)
}
