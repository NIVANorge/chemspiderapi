post_validate_inchikey <- function(inchikey, apikey) {
  # if (!requireNamespace("curl", quietly = TRUE)) {
  #   stop("Package \"curl\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  if (length(inchikey) > 1) {
    warning("This function can only handle individual \"inchikey\" entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider API key (\"apikey\"); returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  # if (nchar(inchikey) != 27) {
  #   warning("The provided \"inchikey\" should be a 27-character vector; not performing API query.", call. = FALSE)
  #   return(data.frame(valid = FALSE))
  # }
  if (length(strsplit(inchikey, split = "-")[[1]]) != 3) {
    warning("The provided \"inchikey\" should be hyphen-divided into three parts; not performing API query.", call. = FALSE)
    return(data.frame(valid = FALSE))
  }
  if (nchar(strsplit(inchikey, split = "-")[[1]][1]) != 14) {
    warning("The first part of the \"inchikey\" should be 14 characters long; not performing API query.", call. = FALSE)
    return(data.frame(valid = FALSE))
  }
  if (nchar(strsplit(inchikey, split = "-")[[1]][2]) != 10) {
    warning("The first part of the \"inchikey\" should be 10 characters long; not performing API query.", call. = FALSE)
    return(data.frame(valid = FALSE))
  }
  if (nchar(strsplit(inchikey, split = "-")[[1]][3]) != 1) {
    warning("The third part of the \"inchikey\" should be 1 character long; not performing API query.", call. = FALSE)
    return(data.frame(valid = FALSE))
  }
  if (substr(strsplit(inchikey, split = "-")[[1]][2], start = 9L, stop = 9L) != "S") {
    warning("This is not a standard \"inchikey\"; performing query regardless.", call. = FALSE)
  }
  curlData <- list(inchikey = inchikey)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- "https://api.rsc.org/compounds/v1/tools/validate/inchikey"
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "POST")
  curl::handle_setopt(curlHandle, postfields = curlData)
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code == 200) {
    return(data.frame(valid = TRUE))
  }
  else {
    return(data.frame(valid = FALSE))
  }
}

