check_format <- function(input, inputFormat, outputFormat) {
  
  if (is.null(input)) {
    stop("Please provide an \"input\".", 
         call. = FALSE)
  }
  
  if (length(input) > 1) {
    stop("Please provide a single \"input\" string.\nFor functional programming, try using chemspider::post_convert() in apply() or purrr::map_chr().", 
         call. = FALSE)
  }
  
  if (!is.character(input)) {
    stop("The provided \"input\" is not a character vector.", 
         call. = FALSE)
  }
  
  if (is.null(inputFormat)) {
    stop("Please provide an \"inputFormat\".", 
         call. = FALSE)
  }
  
  if (is.null(outputFormat)) {
    stop("Please provide an \"outputFormat\".", 
         call. = FALSE)
  }  
  
  if (!any(tolower(inputFormat) %in% 
           c("inchi", "inchikey", "smiles", "mol"))) {
    stop("Please provide a valid \"inputFormat\". See Documentation for details.", 
         call. = FALSE)
  }
  
  if (!any(tolower(outputFormat) %in% 
           c("inchi", "inchikey", "smiles", "mol"))) {
    stop("Please provide a valid \"outputFormat\". See Documentation for details.", 
         call. = FALSE)
  }
  
  if (tolower(inputFormat) == "inchi") {
    check_inchi(input)
  }

  if (tolower(inputFormat) == "inchikey") {
    check_inchikey(input)
  }

  if (tolower(inputFormat) == "smiles") {
    check_smiles(input)
  }

}
