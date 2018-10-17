post_intrinsic_property <- function(property, formula = NULL, complexity = NULL, isotopic = NULL, mass = NULL, range = NULL, orderBy = "recordId", orderDirection = "ascending", apikey) {
  # if (!requireNamespace("curl", quietly = TRUE)) {
  #   stop("Package \"curl\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  # if (!requireNamespace("jsonlite", quietly = TRUE)) {
  #   stop("Package \"jsonlite\" needed for this function to work. Please install it.", call. = FALSE)
  # }
  if (length(property) > 1) {
    warning("This function is meant for single \"property\" entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (sum(tolower(property) %in% c("formula", "molecularweight", "nominalmass", "averagemass", "monoisotopicmass")) < 1) {
    warning("The provided property is not supported; please consult the help file for a list of available properties", call. = FALSE)
    return(NA)
  }
  if (property != "formula" && is.null(mass) || property != "formula" && is.null(range)) {
    warning("Both \"mass\" and \"range\" need to be provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (property == "formula" && is.null(formula)) {
    warning("No \"formula\" provided; returning \"NA\".", call. = )
    return(NA_character_)
  }
  if (sum(is.na(as.double(mass))) > 0) {
    warning("The provided \"mass\" is not a valid (double) number; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (sum(is.na(as.double(range))) > 0) {
    warning("The provided \"range\" is not a valid (double) number; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (!is.null(mass) && mass < 1 || !is.null(mass) && mass > 11000) {
    warning("The provided \"mass\" is outside ChemSpider's settings [1,11000]; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (!is.null(range) && range < 0.0001 || !is.null(range) && range > 100) {
    warning("The provided \"range\" is outside ChemSpider's settings [0.0001,100]; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  if (length(mass) != length(range)) {
    warning("Every \"mass\" needs a \"range\", and vice verca; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  if (nchar(apikey) != 32) {
    warning("Please use a valid 32-character ChemSpider \"apikey\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }

  if (!is.null(complexity) && sum(tolower(complexity) %in% c("any", "single", "multiple")) != 1) {
    warning("Please provide \"complexity\" as either \"any\", \"single\", or \"multiple\"; returning \"NA\".")
    return(NA_character_)
  }
  if (!is.null(isotopic) && sum(tolower(isotopic) %in% c("any", "labeled", "unlabeled")) != 1) {
    warning("Please provide \"isotopic\" as either \"any\", \"labeled\", or \"unlabeled\"; returning \"NA\".")
    return(NA_character_)
  }
  if (is.null(complexity)) {
    complexity <- ""
  }
  if (is.null(isotopic)) {
    isotopic <- ""
  }
  options <- list(complexity = complexity, isotopic = isotopic)
  if (tolower(property) == "formula") {
    curlData <- list(formula = formula, options = options, orderBy = orderBy, orderDirection = orderDirection)
  }
  if (tolower(property) == "molecularweight") {
    molecularWeight <- list(mass = mass, range = range)
    curlData <- list(molecularWeight = molecularWeight, options = options, orderBy = orderBy, orderDirection = orderDirection)
  }
  if (tolower(property) == "nominalmass") {
    nominalMass <- list(mass = mass, range = range)
    curlData <- list(nominalMass = nominalMass, options = options, orderBy = orderBy, orderDirection = orderDirection)
  }
  if (tolower(property) == "averagemass") {
    averageMass <- list(mass = mass, range = range)
    curlData <- list(averageMass = averageMass, options = options, orderBy = orderBy, orderDirection = orderDirection)
  }
  if (tolower(property) == "monoisotopicmass") {
    monoisotopicMass <- list(mass = mass, range = range)
    curlData <- list(monoisotopicMass = monoisotopicMass, options = options, orderBy = orderBy, orderDirection = orderDirection)
  }
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/intrinsicproperty"
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
