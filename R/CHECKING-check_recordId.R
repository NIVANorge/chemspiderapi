.check_recordId <- function(recordId) {
  
  if (is.null(recordId)) {
    stop("No \"recordId\" provided.", 
         call. = FALSE)
  }
  
  if (length(recordId) > 1L) {
    stop("This function can only handle a single \"recordId\" entry.\nFor functional programming, try using it in apply() or purrr::map().", 
         call. = FALSE)
  }
  
  if (!is.numeric(recordId)) {
    stop("Please provide a valid (integer) \"recordId\".", 
         call. = FALSE)
  }
  
  if (is.double(recordId)) {
    recordId <- as.integer(recordId)
    warning("The \"recordId\" was transformed from double to integer.", 
            call. = FALSE)
  }
  
}
