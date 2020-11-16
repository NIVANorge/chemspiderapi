.check_formulas <- function(formulas) {
  
  if (is.null(formulas)) {
    stop("No \"formulas\" provided.", 
         call. = FALSE)
  }
  
  if (!is.character(formulas)) {
    stop("The provided \"formulas\" are not character vectors.", 
         call. = FALSE)
  }
  
  if (length(formulas) < 2L) {
    stop("This is function is meant for multiple \"formulas\".\nFor an individual \"formula\" approach, try chemspiderapi::post_formula().", 
         call. = FALSE)
  }
  
  if (length(formulas) > 100L) {
    stop("This is only meant for up to 100 \"formula\".", 
         call. = FALSE)
  }
  
}
