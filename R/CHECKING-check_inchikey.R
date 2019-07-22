check_inchikey <- function(inchikey) {
  
  if (is.null(inchikey)) {
    stop("No \"inchikey\" provided.", call. = FALSE)
  }
  
  if (length(inchikey) > 1L) {
    stop("This function can only handle a single \"inchikey\" entry.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  
  if (typeof(inchikey) != "character") {
    stop("The \"inchikey\" should be a 27-character string.", call. = FALSE)
  }
  
  if (nchar(inchikey) != 27L) {
    stop("The provided \"inchikey\" is not a 27-character string.", call. = FALSE)
  }
  
  if (length(unlist(strsplit(inchikey, split = "-"))) != 3L) {
    stop("The provided \"inchikey\" must be hyphen-divided into three parts.", call. = FALSE)
  }
  
  if (nchar(unlist(strsplit(inchikey, split = "-"))[1]) != 14L) {
    stop("The first part of the \"inchikey\" should be 14 characters long.", call. = FALSE)
  }
  
  if (nchar(unlist(strsplit(inchikey, split = "-"))[2]) != 10L) {
    stop("The second part of the \"inchikey\" should be 10 characters long.", call. = FALSE)
  }
  
  if (nchar(unlist(strsplit(inchikey, split = "-"))[3]) != 1L) {
    stop("The third part of the \"inchikey\" should be 1 character long.", call. = FALSE)
  }

  if (substr(unlist(strsplit(inchikey, split = "-"))[2], start = 9L, stop = 9L) != "S") {
    warning("This is not a standard \"inchikey\"; performing API query regardless.", call. = FALSE)
  }

}
