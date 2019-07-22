check_order <- function(orderBy, orderDirection) {
  
  if (length(orderBy) > 1) {
    stop("Only a single \"orderBy\" entry is supported.", call. = FALSE)
  }
  
  if (!any(tolower(orderBy) %in% c("recordid", "massdefect", "molecularweight", "referencecount", "datasourcecount", "pubmedcount", "rsccount"))) {
    stop("Please provide a valid input for \"orderBy\".", call. = FALSE)
  }
  
  if (length(orderDirection) > 1) {
    stop("Only a single \"orderDirection\" entry is supported.", call. = FALSE)
  }
  
  if (!any(tolower(orderDirection) %in% c("ascending", "descending"))) {
    stop("Please use either \"ascending\" or \"descending\" as input for \"orderDirection\".", call. = FALSE)
  }
  
}
