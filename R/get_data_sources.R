get_data_sources <- function(apikey) {
  # if (!requireNamespace("httr", quietly = TRUE)) {
  #   stop("Package \"httr\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider API key (\"apikey\"); returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  url <- "https://api.rsc.org/compounds/v1/lookups/datasources"
  result <- httr::GET(url = url, config = httr::add_headers(apikey = apikey))
  if (result$status_code != 200) {
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the validity of the (\"apikey\").")
    return(NA_character_)
  }
  result <- httr::content(result, type = "application/json")
  result <- data.frame(dataSources = unlist(result), stringsAsFactors = FALSE)
  row.names(result) <- NULL
  return(result)
}
