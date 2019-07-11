#' @title Write a .png image file
#' @description Convenience function to write a .png image file, e.g., after downloading with \code{chemspiderapi::get_recordId_image()}.
#' @details A convenience function to write downloaded .png images, as obtained from \code{chemspiderapi::get_recordId_image()}.\cr
#' \cr
#' If successful, it writes a 150 x 150 px .png image (as returned by ChemSpider).
#' @param image Either a base64-encoded character string or a numeric (double) array of the .png image.
#' @param file A character string indicating where and under which name to save the image.
#' @return Writes a .png image file
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Saving the .png file for aspirin
#' recordId <- 2157L
#' apikey <- "a valid 32-character ChemSpider apikey"
#' image <- get_recordId_image(recordId = recordId, apikey = apikey)
#' write_image(image = image, file = "aspirin.png")
#' }
#' @importFrom jsonlite base64_dec
#' @importFrom png readPNG writePNG
#' @export
write_image <- function(image, file) {
  
  if (length(image) > 1) {
    stop("This function can only handle a single \"image\" entry.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  
  if (typeof(image) == "character") {
    image <- jsonlite::base64_dec(image)
    image <- png::readPNG(image)
  }
  
  png::writePNG(image = image, target = file)
}
