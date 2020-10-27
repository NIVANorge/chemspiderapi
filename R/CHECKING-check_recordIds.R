check_recordIds <- function(recordIds) {
  
  if (is.null(recordIds)) {
    stop("No \"recordIds\" provided.", 
         call. = FALSE)
  }
  
  if (length(recordIds) < 2L) {
    stop("This function is meant for multiple \"recordIds\" entries.", 
         call. = FALSE)
  }
  
  if (length(recordIds) > 100L) {
    stop("This function can only handle up to 100 \"recordIds\".", 
         call. = FALSE)
  }
  
  if (!is.numeric(recordIds)) {
    stop("Please provide a valid (integer) \"recordIds\".", 
         call. = FALSE)
  }
  
  if (is.double(recordIds)) {
    recordIds <- as.integer(recordIds)
    warning("The \"recordIds\" were transformed from double to integer.", 
            call. = FALSE)
  }
  
}
