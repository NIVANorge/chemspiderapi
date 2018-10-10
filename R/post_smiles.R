post_smiles <- function(smiles, apikey) {
  # if (!requireNamespace("httr", quietly = TRUE)) {
  #   stop("Package \"httr\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  if (length(smiles) > 1) {
    warning("This function can only handle individual (\"smiles\") entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32 character ChemSpider API key (\"apikey\"); returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  url <- "https://api.rsc.org/compounds/v1/filter/smiles"
  result <- httr::POST(url = url, config = httr::add_headers(apikey = apikey), body = list(smiles = smiles), encode = "json")
  if (result$status_code != 200) {
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the (\"smiles\") and the validity of the API key (\"apikey\").", call. = FALSE)
    return(NA_character_)
  }
  result <- httr::content(result, type = "application/json")
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  return(result)
}
