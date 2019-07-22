check_dataSources <- function(dataSources) {
  
  if (!is.null(dataSources) && length(dataSources) > 20) {
    stop("Only up to 20 different \"dataSources\" are allowed.", call. = FALSE)
  }
  
}
