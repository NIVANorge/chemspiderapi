#' Write a .MOL file
#' 
#' Convenience function to write a .MOL file, e.g., after downloading with \code{chemspiderapi::get_recordId_mol()}.
#' 
#' A convenience function to write .MOL files.\cr
#' \cr
#' If successful, it writes a .mol file.\cr
#' \cr
#' If not successful, it returns \code{NA} without writing a file.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
#' 
#' @param mol A valid .MOL file, e.g., downloaded via \code{chemspiderapi::get_recordId_mol()}.
#' @param file A character string indicating where and under which name to save the .MOL file.
#' @return writes a (human readable) .MOL file
#' @examples 
#' ## Not run:
#' ## Saving the .MOL file for aspirin
#' # recordId <- 2157L
#' # apikey <- "a_valid_ChemSpider_API_key"
#' # mol <- get_recordId_mol(recordId = recordId, apikey = apikey)
#' # write_mol(mol = mol, file = "aspirin.mol")
#' 
#' ## End(Not run)
#' @export
write_mol <- function(mol, file) {
  
  if (length(mol) > 1) {
    warning("This function can only handle individual \"mol\" entries; returing \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA)
  }

  mol <- unname(mol)
  
  if (nchar(mol) < 1) {
    warning("Input is not a valid .mol file; returning \"NA\".\nCarefully check the input to this function.", call. = FALSE)
    return(NA)
  }
  
  writeLines(text = mol, con = file(file), sep = "\n")
}

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
#' ## Not run:
#' ## Saving the .png file for aspirin
#' # recordId <- 2157L
#' # apikey <- "a valid 32-character ChemSpider apikey"
#' # image <- get_recordId_image(recordId = recordId, apikey = apikey)
#' # write_image(image = image, file = "aspirin.png")
#' 
#' ## End(Not run)
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
