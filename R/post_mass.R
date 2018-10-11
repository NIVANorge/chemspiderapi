post_mass <- function(mass, range, dataSources = NULL, orderBy = "recordId", orderDirection = "ascending", apikey) {
  # if (!requireNamespace("httr", quietly = TRUE)) {
  #   stop("Package \"httr\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  if (length(mass) > 1) {
    warning("This function can only handle individual (\"smiles\") entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (is.na(as.double(mass))) {
    warning("The provided \"mass\" is not a valid (double) number; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (is.na(as.double(range))) {
    warning("The provided \"range\" is not a valid (double) number; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (mass < 1 || mass > 11000) {
    warning("The provided \"mass\" is outside ChemSpider's settings [1,11000]; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (range < 0.0001 || range > 100) {
    warning("The provided \"range\" is outside ChemSpider's settings [0.0001,100]; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider (\"apikey\"); returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (!is.null(dataSources) && length(dataSources) > 20) {
    warning("Only up to 20 different \"dataSources\" are allowed, please narrow it down; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (!is.null(dataSources)) {
    dataSources <- paste(dataSources, collapse = ",")
  }
  url <- "https://api.rsc.org/compounds/v1/filter/mass"
  if (is.null(dataSources)) {
    result <- httr::POST(url = url, config = httr::add_headers(apikey = apikey), body = list(mass = mass, range = range, orderBy = orderBy, orderDirection = orderDirection), encode = "json")
  }
  if (!is.null(dataSources)) {
    result <- httr::POST(url = url, config = httr::add_headers(apikey = apikey), body = list(mass = mass, range = range, dataSources = dataSources, orderBy = orderBy, orderDirection = orderDirection), encode = "json")
  }
  if (result$status_code != 200) {
    warning("No valid information was retrieved; returning \"NA\".\nCarfully check the (\"mass\") and its \"range\", \"dataSources\", \"orderBy\", and \"orderDirection\" (if applicable), and the validity of the (\"apikey\").")
    return(NA_character_)
  }
  result <- httr::content(result, type = "application/json")
  result <- as.data.frame(result, stringsAsFactors = FALSE)
  return(result)
}
