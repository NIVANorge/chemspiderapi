#' @title Write a .MOL file
#' @description Convenience function to write a .MOL file, e.g., after downloading with \code{chemspiderapi::get_recordId_mol()}.
#' @details A convenience function to write .MOL files.\cr
#' \cr
#' If successful, it writes a .mol file.\cr
#' @param mol A valid .MOL file, e.g., downloaded via \code{chemspiderapi::get_recordId_mol()}.
#' @param file A character string indicating where and under which name to save the .MOL file.
#' @return writes a (human readable) .MOL file
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Saving the .MOL file for aspirin
#' recordId <- 2157L
#' apikey <- "a_valid_ChemSpider_API_key"
#' mol <- get_recordId_mol(recordId = recordId, apikey = apikey)
#' write_mol(mol = mol, file = "aspirin.mol")
#' }
#' @export
write_mol <- function(mol, file) {
  
  if (length(mol) > 1) {
    stop("This function can only handle individual \"mol\" entries.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }

  mol <- unname(mol)
  
  if (nchar(mol) < 1) {
    stop("Input is not a valid .mol file.\nCarefully check the input to this function.", call. = FALSE)
  }
  
  writeLines(text = mol, con = file(file), sep = "\n")
}
