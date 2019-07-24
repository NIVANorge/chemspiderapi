check_result <- function(result) {
  
  if (ncol(result) == 1L) {
    result <- unlist(result)
    if (length(result) > 1L) {
      result <- unname(result)
    }
  }
  
}
