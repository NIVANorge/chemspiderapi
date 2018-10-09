post_smiles <- function(smiles, apikey) {
  if (length(smiles) > 1) {
    stop("This function can only handle individual (\"smiles\") entries.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
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
  url <- "https://api.rsc.org/compounds/v1/filter/smiles"
  result <- httr::POST(url = url, config = httr::add_headers(apikey = apikey), body = list(smiles = smiles), encode = "json")
  result <- httr::content(result, type = "application/json")
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  if (ncol(result) == 0) {
    warning("No valid information was retrieved, returning NA.\nCarfully check the (\"smiles\") and the validity of the API key (\"apikey\").")
    return(NA_character_)
  }
  return(result)
}
