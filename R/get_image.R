get_image <- function(recordId, apikey, png = FALSE) {
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
    stop("Please use a valid 32 character ChemSpider API key (\"apikey\").", call. = FALSE)
  }
  url <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/image")
  result <- httr::GET(url = url, config = httr::add_headers(apikey = apikey))
  if (result$status_code != 200) {
    warning("No valid results were obtained, returning \"NA\".\nCarefully check the \"queryId\", \"start\" and \"count\" (if applicable), and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  else {
    result <- httr::content(result, type = "application/json")
    result <- as.data.frame(result, stringsAsFactors = FALSE)
    if (png == TRUE) {
      if (!requireNamespace("png", quietly = TRUE)) {
        warning("Package \"png\" needed for this function to work. Please install it.\nSaving image as base64-encoded character string.")
        return(result)
      }
      result <- jsonlite::base64_dec(result[[1]])
      result <- png::readPNG(result)
      return(result)
    }
    else {
      return(result)
    }
  }
}
