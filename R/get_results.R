get_results <- function(queryId, start = NULL, count = NULL, apikey) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package \"httr\" needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  }
  if (length(queryId) > 1) {
    warning("This function can only handle individual (\"queryId\") entries.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return()
  }
  if (!is.null(start) && is.na(as.integer(start))) {
    warning("Please use a valid (integer) value for \"start\".", call. = FALSE)
    return()
  }
  if (!is.null(count) && is.na(as.integer(count))) {
    warning("Please use a valid (integer) value for \"count\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider API key (\"apikey\").", call. = FALSE)
    return()
  }
  url <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/results")
  if (is.null(start) && is.null(count)) {
    result <- httr::GET(url = url, config = httr::add_headers(apikey = apikey))
  }
  if (!is.null(start) && is.null(count)) {
    result <- httr::GET(url = url, config = httr::add_headers(apikey = apikey), query = list(start = start))
  }
  if (is.null(start) && !is.null(count)) {
    result <- httr::GET(url = url, config = httr::add_headers(apikey = apikey), query = list(count = count))
  }
  if (!is.null(start) && !is.null(count)) {
    result <- httr::GET(url = url, config = httr::add_headers(apikey = apikey), query = list(start = start, count = count))
  }
  if (result$status_code != 200) {
    warning("No valid results were obtained, returning \"NA\".\nCarefully check the \"queryId\", \"start\" and \"count\" (if applicable), and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  result <- httr::content(result, type = "application/json")
  if (result$limitedToMaxAllowed == TRUE) {
    warning("The query has resulted in > 10'000 entries. Only the first 10'000 are returned.\nConsider splitting this request using \"start\" and \"count\"", call. = FALSE)
  }
  result <- data.frame(results = unlist(result$results), stringsAsFactors = FALSE)
  return(result)
}
