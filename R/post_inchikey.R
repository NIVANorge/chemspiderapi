post_inchikey <- function(inchikey, apikey) {
  if (length(inchikey) > 1) {
    stop("This function can only handle individual (\"inchikey\") entries.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package \"httr\" needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  }
  if (nchar(apikey) != 32) {
    stop("Please use a valid 32 character ChemSpider API key (\"apikey\").", call. = FALSE)
  }
  url <- "https://api.rsc.org/compounds/v1/filter/inchikey"
  result <- httr::POST(url = url, config = httr::add_headers(apikey = apikey), body = list(inchikey = inchikey), encode = "json")
  result <- httr::content(result, type = "application/json")
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  if (ncol(result) == 0) {
    warning("No valid information was retrieved, returning NA.\nCarfully check the (\"input\"), its format (\"inputFormat\"), the desired \"outputFormat\", and the validity of the API key (\"apikey\").")
    return(NA_character_)
  }
  return(result)
}
