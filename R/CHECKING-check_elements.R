check_elements <- function(includeElements, excludeElements) {
  
  if (is.null(includeElements)) {
    stop("No \"includeElements\" provided.", call. = FALSE)
  }
  
  if (is.null(excludeElements)) {
    stop("No \"excludeElements\" provided.", call. = FALSE)
  }
  
  if (!is.character(includeElements)) {
    stop("\"includeElements\" should consist of character codes for elements.", call. = FALSE)
  }
  
  if (!is.character(excludeElements)) {
    stop("\"excludeElements\" should consist of character codes for elements.", call. = FALSE)
  }
  
  if (length(includeElements) > 15) {
    stop("ChemSpider only supports up to 15 entries in \"includeElements\".", call. = FALSE)
  }
  
  if (length(excludeElements) > 100) {
    stop("ChemSpider only supports up to 100 entries in \"excludeElements\".", call. = FALSE)
  }
  
  if (length(includeElements) == 1) {
    includeElements <- I(includeElements)
  }
  
  if (length(excludeElements) == 1) {
    excludeElements <- I(excludeElements)
  }
  
}