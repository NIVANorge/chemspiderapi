.check_simplify <- function(simplify) {
  
  if (length(simplify) != 1L) {
    stop("Please provide one logical (TRUE/FALSE) to indicate if you want to \"coerce\".", 
         call. = FALSE)
  }
  
  if (!is.logical(simplify)) {
    stop("Please use a logical (TRUE/FALSE) to indicate if you want to \"simplify\".", 
         call. = FALSE)
  }
  
}
