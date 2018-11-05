get_status <- function(queryId, count = TRUE, message = TRUE, apikey) {

  if (length(queryId) > 1L) {
    warning("This function can only handle a single ChemSpider \"queryId\" entry; returning \"NA\".\nMaybe you are looking for chemspideR::get_formula_batch_status() or chemspideR::get_mass_batch_status()?\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }

  if (typeof(queryId) != "character") {
    warning("The ChemSpider \"queryId\" should be a 36-character string; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(queryId) != 36L) {
    warning("Please use a valid 36-character ChemSpider \"queryId\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (length(unlist(strsplit(queryId, split = "-"))) != 5L) {
    warning("The provided ChemSpider \"queryId\" should be hyphen-divided into five parts; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[1]) != 8L) {
    warning("The first part of the ChemSpider \"queryId\" should be 8 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[2]) != 4L) {
    warning("The second part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[3]) != 4L) {
    warning("The third part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[4]) != 4L) {
    warning("The fourth part of the ChemSpider \"queryId\" should be 4 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(unlist(strsplit(queryId, split = "-"))[5]) != 12L) {
    warning("The fifth part of the ChemSpider \"queryId\" should be 12 characters long; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (length(apikey) > 1) {
    warning("This function can only handle a single ChemSpider \"apikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }

  if (typeof(apikey) != "character") {
    warning("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
    return(NA_character_)
  }

  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32-character ChemSpider \"apikey\".", call. = FALSE)
    return(NA_character_)
  }

  curlHeader <- list(`Content-Type` = "", apikey = apikey)

  curlUrl <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/status")

  curlHandle <- curl::new_handle()

  curl::handle_setopt(curlHandle, customrequest = "GET")

  curl::handle_setheaders(curlHandle, .list = curlHeader)

  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)

  if (result$status_code != 200L) {
    warning("Query not yet finalized, please check again in ten seconds.", call. = FALSE)
    return(c(status = "Incomplete"))
  }

  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(result, stringsAsFactors = FALSE)

  if (count == FALSE) {
    result$count <- NULL
  }

  if (message == FALSE) {
    result$message <- NULL
  }

  if (ncol(result) == 1L) {
    result <- unlist(result)
  }

  return(result)
}
