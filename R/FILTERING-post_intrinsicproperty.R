#' POST an intrinsic property
#' 
#' Functionality to POST an intrinsic property to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
#' 
#' 
#' Valid entries for property are: \code{formula}, \code{molecularWeight}, \code{nominalMass}, \code{averageMass}, or \code{monoisotopicMass}.\cr
#' \cr
#' Says ChemSpider:\cr
#' \cr
#' \emph{"Valid values for \code{complexity} are \code{any}, \code{single}, or \code{multiple} whereby a compound with a complexity of multiple has more than one disconnected system in it or a metal atom or ion.\cr
#' \cr
#' Valid values for \code{isotopic} are \code{any}, \code{labeled}, or \code{unlabeled}.\cr
#' \cr
#' You can use \code{orderBy} and \code{orderDirection} to specify the sort order for the results. Valid values for \code{orderBy} are \code{recordId}, \code{massDefect}, \code{molecularWeight}, \code{referenceCount}, \code{dataSourceCount}, \code{pubMedCount}, \code{rscCount}.\cr
#' \cr
#' Valid values for \code{orderDirection} are \code{ascending}, \code{descending}."}\cr
#' \cr
#' If successful, returns the \code{queryId} as character vector.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#' 
#' @param property See Details.
#' @param formula See Details.
#' @param complexity See Details.
#' @param isotopic See Details.
#' @param mass See Details.
#' @param range See Details.
#' @param orderBy A character string indicating by which parameter the results should be ordered; see Details.
#' @param orderDirection A character string indicating in which direction the results should be ordered; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns a query ID as character vector
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/intrinsicproperty}
#' @examples 
#' \dontrun{
#' ## POST an intrinsic property for Aspirin to get a query ID
#' property = "Formula"
#' formula = "C9H8O4"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_intrinsicproperty(property = property, formula = formula, apikey = apikey)
#' 
#' property <- "MolecularWeight"
#' mass <- 180
#' range <- 0.5
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_intrinsicproperty(property = property, mass = mass, range = range, apikey = apikey)
#' }
#' @export
post_intrinsicproperty <- function(property, formula = NULL, complexity = "any", isotopic = "any", mass = NULL, range = NULL, orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  if (is.na(property)) {
    warning("No \"property\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(property) > 1) {
    warning("This function is meant for single \"property\" entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(property) %in% c("formula", "molecularweight", "nominalmass", "averagemass", "monoisotopicmass")) < 1) {
    warning("The provided property is not supported; please consult the help file for a list of available properties", call. = FALSE)
    return(NA_character_)
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
  
  if (length(apikey) > 1L) {
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
  
  if (!is.null(complexity) && sum(tolower(complexity) %in% c("any", "single", "multiple")) != 1) {
    warning("Please provide \"complexity\" as either \"any\", \"single\", or \"multiple\"; returning \"NA\".")
    return(NA_character_)
  }
  
  if (!is.null(isotopic) && sum(tolower(isotopic) %in% c("any", "labeled", "unlabeled")) != 1) {
    warning("Please provide \"isotopic\" as either \"any\", \"labeled\", or \"unlabeled\"; returning \"NA\".")
    return(NA_character_)
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
  
  curl::handle_setopt(curlHandle, customrequest = "POST", postfields = curlData)
  
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)
  
  if (result$status_code != 200L) {
    
    error_message <- "\nNo ChemSpider Error Details were provided."
    
    if (result$status_code == 400L) {
      error_message <- "\nChemSpider Response Error Details: \"400: Bad Request. Check the request you sent and try again.\"."
    }
    
    if (result$status_code == 401L) {
      error_message <- "\nChemSpider Response Error Details: \"401: Unauthorized. Check you have supplied the correct API key and that you have sent it as an HTTP Header called 'apikey'.\"."
    }
    
    if (result$status_code == 404L) {
      error_message <- "\nChemSpider Response Error Details: \"404: Not Found. The requested endpoint URL is not recognized. Change your request and try again.\"."
    }
    
    if (result$status_code == 405L) {
      error_message <- "\nChemSpider Response Error Details: \"405: Method Not Allowed. The verb is incorrect for the endpoint. Change your request and try again.\"."
    }
    
    if (result$status_code == 429L) {
      error_message <- "\nChemSpider Response Error Details: \"429: Too Many Requests. Send fewer requests, or use rate-limiting to slow them down, then try again.\"."
    }
    
    if (result$status_code == 500L) {
      error_message <- "\nChemSpider Response Error Details: \"500: Internal Server Error. Wait and try again.\"."
    }
    
    if (result$status_code == 503L) {
      error_message <- "\nChemSpider Response Error Details: \"503: Service Unavailable. Wait and try again.\"."
    }
    
    message <- paste0("No valid results were obtained; returning \"NA\".\nCarefully check the \"inchikey\" and the validity of the \"apikey\".", error_message)
    
    warning(message, call. = FALSE)
    return(NA_character_)
  }
  
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  return(result)
}
