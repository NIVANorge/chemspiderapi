check_order <- function(orderBy, orderDirection) {
  
  if (!is.null(orderBy) && length(orderBy) > 1) {
    stop("Only a single \"orderBy\" entry is supported.", 
         call. = FALSE)
  }
  
  if (!is.null(orderDirection) && length(orderDirection) > 1) {
    stop("Only a single \"orderDirection\" entry is supported.", 
         call. = FALSE)
  }
  
  if (!is.null(orderBy) && !is.character(orderBy)) {
    stop("Please provide a valid input for \"orderBy\".", 
         call. = FALSE)
  }

  if (!is.null(orderDirection) && !is.character(orderDirection)) {
    stop("Please provide a valid input for \"orderDirection\".", 
         call. = FALSE)
  }
  
  if (!is.null(orderBy) && !any(tolower(orderBy) %in%
                                c("recordid", "massdefect", "molecularweight", "referencecount", "datasourcecount", "pubmedcount", "rsccount"))) {
    stop("Please provide a valid input for \"orderBy\".", 
         call. = FALSE)
  }
  
  if (!is.null(orderDirection) && !any(tolower(orderDirection) %in%
                                       c("ascending", "descending"))) {
    stop("Please use either \"ascending\" or \"descending\" as input for \"orderDirection\".", 
         call. = FALSE)
  }
  
}
