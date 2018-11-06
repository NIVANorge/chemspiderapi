get_mass_batch_status <- function(queryId, count = TRUE, message = TRUE, apikey) {
  if (length(queryId) > 1) {
    warning("This function can only handle individual (\"queryId\") entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(queryId) != 36) {
    warning("Please use a valid 36-character ChemSpider \"queryId\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider \"apikey\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/mass/batch/", queryId, "/status")
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "GET")
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("Query not yet finalized, please check again in 10 seconds.\nCarefully check the validity of the \"apikey\".", call. = FALSE)
    return(c(status = "Incomplete"))
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  if (count == FALSE) {
    result$count <- NULL
  }
  if (message == FALSE) {
    result$message <- NULL
  }
  if (ncol(result) == 1) {
    result <- unlist(result)
  }
  return(result)
}
