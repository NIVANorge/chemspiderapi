get_results <- function(queryId, start = NULL, count = NULL, apikey) {
  # if (!requireNamespace("curl", quietly = TRUE)) {
  #   stop("Package \"curl\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  if (length(queryId) > 1) {
    warning("This function can only handle individual \"queryId\" entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(queryId) != 36) {
    warning("Please use a valid 36-character \"queryId\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (!is.null(start) && is.na(as.integer(start))) {
    warning("Please use a valid integer \"start\" value; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (!is.null(count) && is.na(as.integer(count))) {
    warning("Please use a valid integer \"count\" value; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider (\"apikey\"); returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  curlHeader <- list("Content-Type" = "", "apikey" = apikey)
  if (is.null(start) && is.null(count)) {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/results")
  }
  if (!is.null(start) && is.null(count)) {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/results?start=", start)
  }
  if (is.null(start) && !is.null(count)) {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/results?count=", count)
  }
  if (!is.null(start) && !is.null(count)) {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/results?start=", start, "&count=", count)
  }
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "GET")
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the \"recordId\", the spelling of the \"fields\", and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  if (result$limitedToMaxAllowed == TRUE) {
    warning("The query has resulted in > 10'000 entries. Only the first 10'000 are returned.\nConsider splitting this request using \"start\" and \"count\"", call. = FALSE)
  }
  result <- data.frame(results = result$results, stringsAsFactors = FALSE)
  return(result)
}
