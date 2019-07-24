check_smiles <- function(smiles) {
  
  if (is.null(smiles)) {
    stop("No \"smiles\" provided.", call. = FALSE)
  }
  
  if (length(smiles) > 1) {
    stop("This function can only handle a single \"smiles\" entry.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  
  if (!is.character(smiles)) {
    stop("The provided \"smiles\" is not a character vector.", call. = FALSE)
  }
  
  if (grepl(pattern = "J", x = smiles, ignore.case = TRUE)) {
    stop("The \"smiles\" string contains the letter J, which is not a valid element.",call. = FALSE)
  }
  
}
