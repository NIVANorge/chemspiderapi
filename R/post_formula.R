post_formula <- function(formula, dataSources = "", orderBy = "", orderDirection = "", apikey) {
  if (length(formula) > 1) {
    stop("This function can only handle individual (\"formula\") entries.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
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
  url <- "https://api.rsc.org/compounds/v1/filter/formula"
  result <- httr::POST(url = url, config = httr::add_headers(apikey = apikey), body = list(formula = formula, dataSources = dataSources, orderBy = orderBy, orderDirection = orderDirection), encode = "json")
  if (result$status_code != 200) {
    warning("No valid results were obtained, returning \"NA\".\nCarefully check the \"formula\", \"start\" and \"count\" (if applicable), and the validity of the \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  else {
    result <- httr::content(result, type = "application/json")
    result <- as.data.frame(result, stringsAsFactors = FALSE)
    return(result)
  }
}
