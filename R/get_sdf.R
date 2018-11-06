get_sdf <- function(queryId, apikey) {
  if (length(queryId) > 1) {
    warning("This function can only handle individual \"queryId\" entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(queryId) != 36) {
    warning("Please use a valid 36-character \"queryId\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider (\"apikey\"); returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/results/sdf")
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "GET")
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the \"queryId\" and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result$results)
  return(result)
}
