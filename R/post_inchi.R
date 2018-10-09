post_inchi <- function(inchi, apikey) {
  if (length(inchi) > 1) {
    stop("This function can only handle individual (\"inchi\") entries.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
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
  url <- "https://api.rsc.org/compounds/v1/filter/inchi"
  result <- httr::POST(url = url, config = httr::add_headers(apikey = apikey), body = list(inchi = inchi), encode = "json")
  if (result$status_code != 200) {
    warning("No valid results were obtained, returning \"NA\".\nCarefully check the \"inchi\" and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  else {
    result <- httr::content(result, type = "application/json")
    result <- as.data.frame(result, stringsAsFactors = FALSE)
    return(result)
  }
}
