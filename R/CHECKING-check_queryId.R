check_queryId <- function(queryId) {
  
  if (is.null(queryId)) {
    stop("No valid \"queryId\" provided.", call. = FALSE)
  }
  
  if (length(queryId) > 1L) {
    stop("This function can only handle a single ChemSpider \"queryId\" entry.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  
  if (typeof(queryId) != "character") {
    stop("The ChemSpider \"queryId\" should be a 36-character string.", call. = FALSE)
  }
  
  if (nchar(queryId) != 36L) {
    stop("Please use a valid 36-character ChemSpider \"queryId\".", call. = FALSE)
  }
  
  if (length(unlist(strsplit(queryId, split = "-"))) != 5L) {
    stop("The provided ChemSpider \"queryId\" should be hyphen-divided into five parts.", call. = FALSE)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[1]) != 8L) {
    stop("The first part of the ChemSpider \"queryId\" should be 8 characters long.", call. = FALSE)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[2]) != 4L) {
    stop("The second part of the ChemSpider \"queryId\" should be 4 characters long.", call. = FALSE)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[3]) != 4L) {
    stop("The third part of the ChemSpider \"queryId\" should be 4 characters long.", call. = FALSE)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[4]) != 4L) {
    stop("The fourth part of the ChemSpider \"queryId\" should be 4 characters long.", call. = FALSE)
  }
  
  if (nchar(unlist(strsplit(queryId, split = "-"))[5]) != 12L) {
    stop("The fifth part of the ChemSpider \"queryId\" should be 12 characters long.", call. = FALSE)
  }

}
