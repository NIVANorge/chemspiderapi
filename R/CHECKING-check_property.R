check_property <- function(property) {
  
  if (is.null(property)) {
    stop("No \"property\" provided.", 
         call. = FALSE)
  }
  
  if (!is.character(property)) {
    stop("The provided \"property\" is not a character vector.", 
         call. = FALSE)
  }
  
  if (length(property) > 1) {
    stop("This function is meant for single \"property\" entries.\nFor functional programming, try using it in apply() or purrr::map().", 
         call. = FALSE)
  }
  
  if (!any(tolower(property) %in% 
           c("formula", "molecularweight", "nominalmass", "averagemass", "monoisotopicmass"))) {
    stop("The provided property is not supported; please consult the help file for a list of available properties", 
         call. = FALSE)
  }
  
}
