post_convert <- function(input, inputFormat, outputFormat, apikey) {
  if (length(input) > 1) {
    stop("This function can only handle individual (\"input\") entries.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package \"httr\" needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  }
  if (nchar(apikey) != 32) {
    stop("Please use a valid 32 character ChemSpider API key (\"apikey\").", call. = FALSE)
  }
  if (sum(tolower(inputFormat) %in% c("inchi", "inchikey", "smiles", "mol")) < 1) {
    warning("This entry of \"inputFormat\" is not a valid ChemSpider input format; returning \"NA\".\nMaybe a spelling error?", call. = FALSE)
    return(NA_character_)
  }
  if (sum(tolower(outputFormat) %in% c("inchi", "inchikey", "smiles", "mol")) < 1) {
    stop("This entry of \"outputFormat\" is not a valid ChemSpider output format.\nMaybe a spelling error?", call. = FALSE)
  }
  curlData <- list("input" = input, "inputFormat" = inputFormat, "outputFormat" = outputFormat)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  curlHeader <- list("Content-Type" = "", "apikey" = apikey)
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
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  return(result)
}
