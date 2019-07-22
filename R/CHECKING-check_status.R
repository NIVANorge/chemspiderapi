check_status <- function(status) {
  
  if (is.null(status)) {
    stop("No ChemSpider query \"status\" provided.", call. = FALSE)
  }
  
  if (!is.character(status)) {
    stop("The provided ChemSpider query status needs to be a character vector.", call. = FALSE)
  }
  
  if (status != "Complete") {
    stop("Query computation not yet complete.", call. = FALSE)
  }

}
