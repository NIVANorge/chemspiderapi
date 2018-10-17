post_inchi <- function(inchi, apikey) {
  if (length(inchi) > 1) {
    warning("This function can only handle a single \"inchi\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (substr(tolower(inchi), start = 1L, stop = 7L) != "inchi=1") {
    warning("This is not a valid \"inchi\" string because \"InChI=1\" is missing in the beginning; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (substr(tolower(inchi), start = 8L, stop = 8L) != "s") {
    warning("This is not a standard \"inchi\" string; performing API query regardless.", call. = FALSE)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider API key (\"apikey\").", call. = FALSE)
    return(NA_character_)
  }
  curlData <- list(inchi = inchi)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/inchi"
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "POST")
  curl::handle_setopt(curlHandle, postfields = curlData)
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("No valid results were obtained; returning \"NA\".\nCarefully check the \"inchi\" string and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  return(result)
}
