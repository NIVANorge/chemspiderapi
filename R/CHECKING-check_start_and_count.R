.check_start_and_count <- function(start, count) {
  
  if (!is.null(start) && !is.numeric(start)) {
    stop("Please use a valid integer \"start\" value.", 
         call. = FALSE)
  }
  
  if (!is.null(count) && !is.numeric(count)) {
    stop("Please use a valid integer \"count\" value.", 
         call. = FALSE)
  }
  
  if (!is.null(start) && is.double(start)) {
    start <- as.integer(start)
    warning("The \"start\" value has been transformed from double to integer.",
            call. = FALSE)
  }
  
  if (!is.null(count) && is.double(count)) {
    count <- as.integer(count)
    warning("The \"count\" value has been transformed from double to integer.",
            call. = FALSE)
  }
  
}
