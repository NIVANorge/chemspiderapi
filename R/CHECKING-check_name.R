check_name <- function(name) {
  
  if (is.null(name)) {
    stop("No \"name\" provided.", call. = FALSE)
  }
  
  if (length(name) > 1) {
    stop("This function can only handle a single \"name\" entry.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  
  if (!is.character(name)) {
    stop("The prodived \"name\" is not a character vector.", call. = FALSE)
  }
  
}
