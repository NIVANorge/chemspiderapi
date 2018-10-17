post_inchikey <- function(inchikey, apikey) {
  if (length(inchikey) > 1) {
    warning("This function can only handle individual single \"inchikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (length(strsplit(inchikey, split = "-")[[1]]) != 3) {
    warning("The provided \"inchikey\" should be hyphen-divided into three parts; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(strsplit(inchikey, split = "-")[[1]][1]) != 14) {
    warning("The first part of the \"inchikey\" should be 14 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(strsplit(inchikey, split = "-")[[1]][2]) != 10) {
    warning("The first part of the \"inchikey\" should be 10 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(strsplit(inchikey, split = "-")[[1]][3]) != 1) {
    warning("The third part of the \"inchikey\" should be 1 character long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (substr(strsplit(inchikey, split = "-")[[1]][2], start = 9L, stop = 9L) != "S") {
    warning("This is not a standard \"inchikey\"; performing API query regardless.", call. = FALSE)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32 character ChemSpider API key (\"apikey\").", call. = FALSE)
    return(NA_character_)
  }
  curlData <- list(inchikey = inchikey)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/inchikey"
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "POST")
  curl::handle_setopt(curlHandle, postfields = curlData)
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("No valid results were obtained; returning \"NA\".\nCarefully check the \"inchikey\" and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  return(result)
}
