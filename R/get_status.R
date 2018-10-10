get_status <- function(queryId, apikey) {
  # if (!requireNamespace("httr", quietly = TRUE)) {
  #   stop("Package \"httr\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  if (length(queryId) > 1) {
    warning("This function can only handle individual (\"queryId\") entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider API key (\"apikey\"); returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  url <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/status")
  result <- httr::GET(url = url, config = httr::add_headers(apikey = apikey))
  if (result$status_code != 200) {
    warning("Query not yet finalized, please check again in 10 seconds.\nCarefully check the validity of the \"apikey\".", call. = FALSE)
    return(data.frame(status = "Incomplete", stringsAsFactors = FALSE))
  }
  result <- httr::content(result, type = "application/json")
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  return(result)
}
