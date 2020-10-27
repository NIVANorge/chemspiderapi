check_formula <- function(formula) {
  
  if (is.null(formula)) {
    stop("No \"formula\" provided.", 
         call. = FALSE)
  }
  
  if (length(formula) > 1) {
    stop("This function can only handle individual \"formula\" entries.\nFor functional programming, try using it in apply() or purrr::map().", 
         call. = FALSE)
  }
  
  if (!is.character(formula)) {
    stop("The provided \"formula\" is not a character vector.", 
         call. = FALSE)
  }
  
}
