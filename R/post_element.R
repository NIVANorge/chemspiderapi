post_element <- function(includeElements, excludeElements, includeAll = FALSE, complexity = "any", isotopic = "any", orderBy = "recordId", orderDirection = "ascending", apikey) {
  # if (!requireNamespace("httr", quietly = TRUE)) {
  #   stop("Package \"httr\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  if (length(includeElements) > 15) {
    warning("ChemSpider only supports up to 15 entries in \"includeElements\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (length(excludeElements) > 100) {
    warning("ChemSpider only supports up to 100 entries in \"excludeElements\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (sum(tolower(complexity) %in% c("any", "single", "multiple")) != 1) {
    warning("The provided complexity is not \"any\", \"single\", or \"multiple\"; returning NA.", call. = FALSE)
    return(NA_character_)
  }
  if (sum(tolower(isotopic) %in% c("any", "labeled", "unlabeled")) != 1) {
    warning("The provided isotopic is not \"any\", \"labeled\", or \"unlabeled\"; returning NA.", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider (\"apikey\"); returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (length(includeElements) == 1) {
    includeElements <- I(includeElements)
  }
  if (length(excludeElements) == 1) {
    excludeElements <- I(excludeElements)
  }
  options <- list(includeAll = includeAll, complexity = complexity, isotopic = isotopic)
  curlData <- list(includeElements = includeElements, excludeElements = excludeElements, options = options, orderBy = orderBy, orderDirection = orderDirection)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/element"
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
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  return(result)
}
