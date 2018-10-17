get_image <- function(recordId, apikey, png = FALSE) {
  if (length(recordId) > 1) {
    warning("This function can only handle a single \"recordId\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (is.na(as.integer(recordId))) {
    warning("Please use a valid (integer) \"recordId\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider \"apikey\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/image")
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "GET")
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("No valid results were obtained; returning \"NA\".\nCarefully check the \"recordId\" and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  if (png == TRUE) {
    result <- jsonlite::base64_dec(result)
    result <- png::readPNG(result)
  }
  return(result)
}
