.check_coerce <- function(coerce) {
  
  if (!is.logical(coerce)) {
    stop("Please use a logical to indicate if you want to \"coerce\".", 
         call. = FALSE)
  }
  
}
