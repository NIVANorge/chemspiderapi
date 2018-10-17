get_mol <- function(recordId, apikey) {
  # if (!requireNamespace("curl", quietly = TRUE)) {
  #   stop("Package \"curl\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  if (length(recordId) > 1) {
    warning("This function can only handle individual ChemSpider IDs (\"recordId\"); returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (is.na(as.integer(recordId))) {
    warning("The ChemSpider ID (\"recordId\") must be an integer number; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please include a valid 32 character ChemSpider \"apikey\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/mol")
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "GET")
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("No valid Mol/SDF file was retrieved; returning \"NA\".\nCarfully check the ChemSpider ID (\"recordId\") and the validity of the API key (\"apikey\").")
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  if (nchar(result) < 1) {
    warning("No valid Mol/SDF file was retrieved; returning \"NA\".\nCarfully check the ChemSpider ID (\"recordId\") and the validity of the API key (\"apikey\").")
    return(NA_character_)
  }
  return(result)
}
