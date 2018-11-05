get_results <- function(queryId, start = NULL, count = NULL, apikey, status) {

  if (length(queryId) > 1L) {
    warning("This function can only handle individual ChemSpider \"queryId\" entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_integer_)
  }

  if (nchar(queryId) != 36L) {
    warning("Please use a valid 36-character ChemSpider \"queryId\"; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (is.null(status)) {
    warning("No ChemSpider query \"status\" provided; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (status != "Complete") {
    warning("Query computation not yet completet; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (!is.null(start) && is.na(as.integer(start))) {
    warning("Please use a valid integer \"start\" value; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (!is.null(count) && is.na(as.integer(count))) {
    warning("Please use a valid integer \"count\" value; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32-character ChemSpider \"apikey\"; returning \"NA\".", call. = FALSE)
    return(NA_integer_)
  }

  curlHeader <- list(`Content-Type` = "", apikey = apikey)

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

  if (result$status_code != 200L) {
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the \"recordId\", the spelling of the \"fields\", and the validity of the \"apikey\".", call. = FALSE)
    return(NA_integer_)
  }

  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)

  if (result$limitedToMaxAllowed == TRUE) {
    warning("The query has resulted in > 10'000 entries. Only the first 10'000 are returned.\nConsider splitting this request using \"start\" and \"count\".", call. = FALSE)
  }

  result <- data.frame(results = result$results, stringsAsFactors = FALSE)

  if (ncol(result) == 1L) {
    result <- unlist(result)
  }

  return(result)
}
