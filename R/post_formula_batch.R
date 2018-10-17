post_formula_batch <- function(formulas, dataSources = NULL, orderBy = "recordId", orderDirection = "ascending", apikey) {
  # if (!requireNamespace("curl", quietly = TRUE)) {
  #   stop("Package \"curl\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  if (length(formulas) == 1) {
    warning("This is ment for multiple \"formula\"; returning \"NA\".\nFor an individual \"formula\" approach, try chemspideR::post_formula().", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32 character ChemSpider API key (\"apikey\").", call. = FALSE)
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
    warning("No valid results were obtained, returning \"NA\".\nCarefully check the \"formula\", \"start\" and \"count\" (if applicable), and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  return(result)
}
