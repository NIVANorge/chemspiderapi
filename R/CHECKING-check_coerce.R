.check_coerce <- function(coerce) {
  
  if (length(coerce) != 1L) {
    stop("Please provide one logical (TRUE/FALSE) to indicate if you want to \"coerce\".", 
         call. = FALSE)
  }
  
  if (!is.logical(coerce)) {
    stop("Please use a logical (TRUE/FALSE) to indicate if you want to \"coerce\".", 
         call. = FALSE)
  }
  
}
