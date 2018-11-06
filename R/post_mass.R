post_mass <- function(mass, range, dataSources = NULL, orderBy = "recordId", orderDirection = "ascending", apikey) {
  if (length(mass) > 1) {
    warning("This function can only handle a single (\"mass\") entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (is.na(as.double(mass))) {
    warning("The provided \"mass\" is not a valid (double) number; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (mass < 1 || mass > 11000) {
    warning("The provided \"mass\" is outside ChemSpider's settings [1,11000]; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (length(range) > 1) {
    warning("This function can only handle a single (\"range\") entry; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (is.na(as.double(range))) {
    warning("The provided \"range\" is not a valid (double) number; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (range < 0.0001 || range > 100) {
    warning("The provided \"range\" is outside ChemSpider's settings [0.0001,100]; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (!is.null(dataSources) && length(dataSources) > 20) {
    warning("Only up to 20 different \"dataSources\" are allowed; returning \"NA\".", call. = FALSE)
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
    warning("Please use a valid 32-character ChemSpider (\"apikey\"); returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (!is.null(dataSources)) {
    if (length(dataSources) == 1) {
      dataSources <- I(dataSources)
    }
    curlData <- list(mass = mass, range = range, dataSources = dataSources, orderBy = orderBy, orderDirection = orderDirection)
  }
  else {
    curlData <- list(mass = mass, range = range, orderBy = orderBy, orderDirection = orderDirection)
  }
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/mass"
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
