check_inchi <- function(inchi) {
  
  if (is.null(inchi)) {
    stop("No \"inchi\" string provided.", call. = FALSE)
  }
  
  if (!is.character(inchi)) {
    stop("The provided \"inchi\" string is not a character vector.", call. = FALSE)
  }
  
  if (length(inchi) > 1) {
    stop("This function can only handle a single \"inchi\" entry.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  
  if (!grepl(pattern = "InChI=", x = inchi)) {
    stop("This is not a valid \"inchi\" string because \"InChI=\" is missing in the beginning.", call. = FALSE)
  }
  
  if (substr(x = inchi, start = 8L, stop = 8L) != "S") {
    warning("This is not a standard \"inchi\" string; performing API query regardless.", call. = FALSE)
  }
  
}
