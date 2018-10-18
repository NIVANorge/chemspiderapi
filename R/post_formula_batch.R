post_formula_batch <- function(formulas, dataSources = NULL, orderBy = "recordId", orderDirection = "ascending", apikey) {
  if (length(formulas) == 1) {
    warning("This is function is meant for multiple \"formula\"; returning \"NA\".\nFor an individual \"formula\" approach, try chemspideR::post_formula().", call. = FALSE)
    return(NA_character_)
  }
  if (length(formulas) > 100) {
    warning("This is only meant for up to 100 \"formula\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (sum(tolower(orderBy) %in% c("recordid", "massdefect", "molecularweight", "referencecount", "datasourcecount", "pubmedcount", "rsccount")) != 1) {
    warning("No valid \"orderBy\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (sum(tolower(orderDirection) %in% c("ascending", "descending")) != 1) {
    warning("No valid \"orderDirection\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32 character ChemSpider \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  if (length(dataSources) == 1) {
    dataSources <- I(dataSources)
  }
  dataSources <- paste(dataSources, collapse = ",")
  curlData <- list(formulas = formulas, dataSources = dataSources, orderBy = orderBy, orderDirection = orderDirection)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/formula/batch"
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "POST")
  curl::handle_setopt(curlHandle, postfields = curlData)
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("No valid results were obtained, returning \"NA\".\nCarefully check the \"formulas\", \"dataSources\", \"orderBy\" and \"orderDirection\" (if applicable), and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  return(result)
}
