.check_simplify <- function(simplify) {
  
  if (!is.logical(simplify)) {
    stop("Please use a logical to indicate if you want to \"simplify\".", 
         call. = FALSE)
  }
  
}
