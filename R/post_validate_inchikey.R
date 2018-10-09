post_validate_inchikey <- function(inchikey, apikey) {
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
  url <- "https://api.rsc.org/compounds/v1/tools/validate/inchikey"
  result <- httr::POST(url = url, config = httr::add_headers(apikey = apikey), body = list(inchikey = inchikey), encode = "json")
  if (result$status_code == 200) {
    return(data.frame(valid = TRUE))
  }
  else {
    return(data.frame(valid = FALSE))
  }
}
