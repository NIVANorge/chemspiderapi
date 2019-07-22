check_apikey <- function(apikey) {
  
  if (length(apikey) > 1L) {
    stop("This function can only handle a single ChemSpider \"apikey\" entry.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  
  if (!is.character(apikey)) {
    stop("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
  }
  
  if (nchar(apikey) != 32L) {
    stop("Please use a valid 32-character ChemSpider \"apikey\".", call. = FALSE)
  }
}