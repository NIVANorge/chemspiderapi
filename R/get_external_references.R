get_external_references <- function(recordId, dataSources = NULL, apikey, source = TRUE, sourceUrl = TRUE, externalId = TRUE, externalUrl = TRUE) {
  if (length(recordId) > 1) {
    warning("This function can only handle a single \"recordId\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (is.na(as.integer(recordId))) {
    warning("Please use a valid \"recordId\"; returning NA.", call. = FALSE)
    return(NA_character_)
  }
  if (!is.null(dataSources)) {
    dataSources <- paste(dataSources, collapse = ",")
  }
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  if (!is.null(dataSources)) {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/externalreferences?dataSources=", dataSources)
  }
  else {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/externalreferences")
  }
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "GET")
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the \"recordId\", the \"dataSources\" (if applicable), and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- result[[1]]
  if (source == FALSE) {
    result$source <- NULL
  }
  if (sourceUrl == FALSE) {
    result$sourceUrl <- NULL
  }
  if (externalId == FALSE) {
    result$externalId <- NULL
  }
  if (externalUrl == FALSE) {
    result$externalUrl <- NULL
  }
  if (ncol(result) == 1) {
    result <- unlist(result)
  }
  return(result)
}
