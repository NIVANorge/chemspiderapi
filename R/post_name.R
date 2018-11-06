post_name <- function(name, orderBy = "recordId", orderDirection = "ascending", apikey) {
  if (length(name) > 1) {
    warning("This function can only handle a single \"name\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (length(orderBy) > 1) {
    warning("Only a single \"orderBy\" entry is supported; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (sum(tolower(orderBy) %in% c("recordid", "massdefect", "molecularweight", "referencecount", "datasourcecount", "pubmedcount", "rsccount")) != 1) {
    warning("Please provide a valid input for \"orderBy\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (length(orderDirection) > 1) {
    warning("Only a single \"orderDirection\" entry is supported; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (sum(tolower(orderDirection) %in% c("ascending", "descending")) != 1) {
    warning("Please use either \"ascending\" or \"descending\" as input for \"orderDirection\"; returning NA.", call. = FALSE)
    return(NA_character_)
  }
  if (length(apikey) > 1) {
    warning("Only a single \"apikey\" is supported; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32 character ChemSpider \"apikey\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  curlData <- list(name = name, orderBy = orderBy, orderDirection = orderDirection)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/name"
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "POST")
  curl::handle_setopt(curlHandle, postfields = curlData)
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the \"name\", \"orderBy\" and \"orderDirection\" (if applicable), and the validity of the (\"apikey\").", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  return(result)
}
