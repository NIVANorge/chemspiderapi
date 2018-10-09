get_external_references <- function(recordId, dataSources = "", apikey) {
  if (length(recordId) > 1) {
    stop("This function can only handle individual (\"recordId\") entries.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package \"httr\" needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  }
  if (is.na(as.integer(recordId))) {
    stop("Please use a valid (\"recordId\").", call. = FALSE)
  }
  dataSources <- tolower(dataSources)
  dataSources2 <- paste0(dataSources, collapse = ",")
  url <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/externalreferences")
  result <- httr::GET(url = url, config = httr::add_headers(apikey = apikey), query = list(dataSources = dataSources2))
  if (result$status_code != 200) {
    warning("No valid information was retrieved, returning NA.\nCarfully check the (\"recordId\"), the desired (\"dataSources\"), and the validity of the API key (\"apikey\").")
    return(NA_character_)
  }
  else {
    result <- httr::content(result, type = "application/json")
    result2 <- data.frame(source = character(), sourceUrl = character(), externalId = character(), externalUrl = character(), stringsAsFactors = FALSE)
    for (i in 1:length(result[[1]])) {
      result2[i, ] <- sapply(result[[1]][i], FUN = as.character)
    }
    if (length(unique(result2$source)) < length(dataSources)) {
      warning("One (or more) \"dataSources\" were not found. Maybe a spelling mistake?", call. = FALSE)
    }
    return(result)
  }
}
