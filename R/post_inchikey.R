post_inchikey <- function(inchikey, apikey) {

  if (length(inchikey) > 1L) {
    warning("This function can only handle a single \"inchikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }

  if (typeof(inchikey) != "character") {
    warning("The \"inchikey\" should be a 27-character string; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(inchikey) != 27L) {
    warning("The provided \"inchikey\" is not a 27-character string; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (length(unlist(strsplit(inchikey, split = "-"))) != 3L) {
    warning("The provided \"inchikey\" should be hyphen-divided into three parts; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(inchikey, split = "-"))[1]) != 14L) {
    warning("The first part of the \"inchikey\" should be 14 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(inchikey, split = "-"))[2]) != 10L) {
    warning("The second part of the \"inchikey\" should be 10 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(inchikey, split = "-"))[3]) != 1L) {
    warning("The third part of the \"inchikey\" should be 1 character long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (length(apikey) > 1L) {
    warning("This function can only handle a single \"apikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }

  if (typeof(apikey) != "character") {
    warning("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32 character ChemSpider \"apikey\".", call. = FALSE)
    return(NA_character_)
  }

  if (substr(unlist(strsplit(inchikey, split = "-"))[2], start = 9L, stop = 9L) != "S") {
    warning("This is not a standard \"inchikey\"; performing API query regardless.", call. = FALSE)
  }

  curlData <- list(inchikey = inchikey)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)

  curlHeader <- list(`Content-Type` = "", apikey = apikey)

  curlUrl <- "https://api.rsc.org/compounds/v1/filter/inchikey"

  curlHandle <- curl::new_handle()

  curl::handle_setopt(curlHandle, customrequest = "POST", postfields = curlData)

  curl::handle_setheaders(curlHandle, .list = curlHeader)

  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)

  if (result$status_code != 200L) {
    warning("No valid results were obtained; returning \"NA\".\nCarefully check the \"inchikey\" and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }

  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)

  return(result)
}
