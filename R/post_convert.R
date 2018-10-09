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
  inputFormat <- tolower(inputFormat)
  if (sum(inputFormat %in% c("inchi", "inchikey", "smiles", "mol")) < 1) {
    stop("This entry of \"inputFormat\" is not a valid ChemSpider input format.\nMaybe a spelling error?", call. = FALSE)
  }
  outputFormat <- tolower(outputFormat)
  if (sum(outputFormat %in% c("inchi", "inchikey", "smiles", "mol")) < 1) {
    stop("This entry of \"outputFormat\" is not a valid ChemSpider output format.\nMaybe a spelling error?", call. = FALSE)
  }
  url <- "https://api.rsc.org/compounds/v1/tools/convert"
  result <- httr::POST(url = url, config = httr::add_headers(apikey = apikey), body = list(input = input, inputFormat = inputFormat, outputFormat = outputFormat), encode = "json")
  result <- httr::content(result, type = "application/json")
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  if (ncol(result) == 0) {
    warning("No valid information was retrieved, returning NA.\nCarfully check the (\"input\"), its format (\"inputFormat\"), the desired \"outputFormat\", and the validity of the API key (\"apikey\").")
    return(NA_character_)
  }
  return(result)
}
