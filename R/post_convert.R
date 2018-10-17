post_convert <- function(input, inputFormat, outputFormat, apikey) {
  # if (!requireNamespace("curl", quietly = TRUE)) {
  #   stop("Package \"curl\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
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
    stop("This entry of \"outputFormat\" is not a valid ChemSpider output format.\nMaybe a spelling error?", call. = FALSE)
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
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the \"name\", \"orderBy\" and \"orderDirection\" (if applicable), and the validity of the (\"apikey\").", call. = FALSE)
    return(NA_character_)
  }
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.character(result)
  return(result)
}
