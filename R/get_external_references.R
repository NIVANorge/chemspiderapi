get_external_references <- function(recordId, dataSources = NULL, apikey) {
  # if (!requireNamespace("curl", quietly = TRUE)) {
  #   stop("Package \"curl\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  if (length(recordId) > 1) {
    warning("This function can only handle individual \"recordId\" entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (is.na(as.integer(recordId))) {
    warning("Please use a valid (\"recordId\").", call. = FALSE)
    return(NA_character_)
  }
  if (!is.null(dataSources)) {
    dataSources <- paste(dataSources, collapse = ",")
  }
  curlHeader <- list("Content-Type" = "", "apikey" = apikey)
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
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the \"recordId\", the desired \"dataSources\", and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result[[1]], stringsAsFactors = FALSE)
  if (length(unique(result$source)) < length(dataSources)) {
    warning("One (or more) \"dataSources\" were not found. Maybe a spelling mistake?", call. = FALSE)
  }
  return(result)
}
