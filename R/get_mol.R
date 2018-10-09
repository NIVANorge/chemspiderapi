get_mol <- function(recordId, apikey) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package \"httr\" needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  }
  if (length(recordId) > 1) {
    stop("This function can only handle individual ChemSpider IDs (\"recordId\").\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  if (is.na(as.integer(recordId))) {
    stop("The ChemSpider ID (\"recordId\") must be an integer number.", call. = FALSE)
  }
  if (nchar(apikey) != 32) {
    stop("Please include a valid 32 character ChemSpider API key (\"apikey\").", call. = FALSE)
  }
  url <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/mol")
  result <- httr::GET(url = url, config = httr::add_headers(apikey = apikey))
  result <- httr::content(result, type = "application/json")
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  if (nchar(result) < 1) {
    warning("No valid MOL file was retrieved, returning NA.\nCarfully check the ChemSpider ID (\"recordId\") and the validity of the API key (\"apikey\").")
    return(NA_character_)
  }
  else{
    return(result)
  }
}
