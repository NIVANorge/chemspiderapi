get_data_sources <- function(apikey) {
  # if (!requireNamespace("httr", quietly = TRUE)) {
  #   stop("Package \"httr\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider API key (\"apikey\"); returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- "https://api.rsc.org/compounds/v1/lookups/datasources"
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "GET")
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the validity of the \"apikey\".")
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.character(result)
  return(result)
}
