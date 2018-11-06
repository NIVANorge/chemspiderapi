post_convert <- function(input, inputFormat, outputFormat, apikey) {
  if (length(input) > 1) {
    warning("This function can only handle individual (\"input\") entries.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32 character ChemSpider API key (\"apikey\").", call. = FALSE)
    return(NA_character_)
  }
  if (sum(tolower(inputFormat) %in% c("inchi", "inchikey", "smiles", "mol")) != length(inputFormat)) {
    warning("This entry of \"inputFormat\" is not a valid ChemSpider input format; returning \"NA\".\nMaybe a spelling error?", call. = FALSE)
    return(NA_character_)
  }
  if (sum(tolower(outputFormat) %in% c("inchi", "inchikey", "smiles", "mol")) != length(outputFormat)) {
    warning("This entry of \"outputFormat\" is not a valid ChemSpider output format.\nMaybe a spelling error?", call. = FALSE)
    return(NA_character_)
  }
  if (tolower(inputFormat) == "inchi" && tolower(substr(input, start = 1L, stop = 7L)) != "inchi=1") {
    warning("Provided \"input\" is not a valid InChI string; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (tolower(inputFormat) == "inchikey" && nchar(input) != 27) {
    warning("The provided \"inchikey\" should be a 27-character vector; not performing API query.", call. = FALSE)
    return(FALSE)
  }
  if (tolower(inputFormat) == "inchikey" && length(strsplit(input, split = "-")[[1]]) != 3) {
    warning("The provided \"inchikey\" should be hyphen-divided into three parts; not performing API query.", call. = FALSE)
    return(FALSE)
  }
  if (tolower(inputFormat) == "inchikey" && nchar(strsplit(input, split = "-")[[1]][1]) != 14) {
    warning("The first part of the \"inchikey\" should be 14 characters long; not performing API query.", call. = FALSE)
    return(FALSE)
  }
  if (tolower(inputFormat) == "inchikey" && nchar(strsplit(input, split = "-")[[1]][2]) != 10) {
    warning("The first part of the \"inchikey\" should be 10 characters long; not performing API query.", call. = FALSE)
    return(FALSE)
  }
  if (tolower(inputFormat) == "inchikey" && nchar(strsplit(input, split = "-")[[1]][3]) != 1) {
    warning("The third part of the \"inchikey\" should be 1 character long; not performing API query.", call. = FALSE)
    return(FALSE)
  }
  if (tolower(inputFormat) == "inchikey" && substr(strsplit(input, split = "-")[[1]][2], start = 9L, stop = 9L) != "S") {
    warning("This is not a standard \"inchikey\"; performing query regardless.", call. = FALSE)
  }
  curlData <- list(input = input, inputFormat = inputFormat, outputFormat = outputFormat)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- "https://api.rsc.org/compounds/v1/tools/convert"
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "POST")
  curl::handle_setopt(curlHandle, postfields = curlData)
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  if (result$status_code != 200) {
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the \"input\", \"inputFormat\", \"outputFormat\", if the conversion is supported (!), and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  return(result)
}
