post_mass <- function(mass = 0, range = 0, dataSources = "", orderBy = "", orderDirection = "", apikey) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package \"httr\" needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  }
  if (length(mass) > 1) {
    warning("This function can only handle individual (\"smiles\") entries.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32 character ChemSpider API key (\"apikey\"); returning NA.", call. = FALSE)
    return(NA_character_)
  }
  if (mass < 1 || mass > 11000) {
    warning("The provided \"mass\" is outside ChemSpider's settings [1:11000]; returning NA.", call. = FALSE)
    return(NA_character_)
  }
  if (range < 0.0001 || range > 100) {
    warning("The provided \"range\" is outside ChemSpider's settings [0.0001:100]; returning NA.", call. = FALSE)
    return(NA_character_)
  }
  url <- "https://api.rsc.org/compounds/v1/filter/mass"
  result <- httr::POST(url = url, config = httr::add_headers(apikey = apikey), body = list(mass = mass, range = range, orderBy = orderBy, orderDirection = orderDirection), encode = "json")
  if (result$status_code != 200) {
    warning("No valid information was retrieved, returning NA.\nCarfully check the (\"mass\"), its \"range\", and the validity of the API key (\"apikey\").")
    return(NA_character_)
  }
  result <- httr::content(result, type = "application/json")
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  return(result)
}
