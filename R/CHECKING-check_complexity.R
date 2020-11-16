.check_complexity <- function(complexity) {
  
  if (length(complexity) != 1L) {
    stop("Only one \"complexity\" is possible.", 
         call. = FALSE)
  }
  
  if (!any(tolower(complexity) %in% 
           c("any", "single", "multiple"))) {
    stop("The provided complexity is not \"any\", \"single\", or \"multiple\".", 
         call. = FALSE)
  }
  
}
