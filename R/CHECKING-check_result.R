.check_result <- function(result) {
  
  if (ncol(result) == 1L) {
    result <- unlist(result, recursive = TRUE, use.names = FALSE)
  }
  
  result
  
}
