#' POST a monoisotopic mass and its range to obtain a query ID
#' 
#' Functionality to POST an atomic mass and its range to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspiderapi::get_queryId_results()}.
#' 
#' Says Chemspider:\cr
#' \cr
#' \emph{"Submit mass as a double between 1 and 11000 Atomic Mass Units, and a range between 0.0001 and 100 via POST. For example, if you specify a mass of 40 and a range of 5, all monoisotopic masses between 35 and 45 are searched.\cr
#' \cr
#' [...] If \code{dataSources} is not specified, all known sources are searched. This will take longer.\cr
#' \cr
#' Optionally, you can also submit \code{orderBy} and \code{orderDirection} to specify the sort order for the results. If you do not specify a value for \code{orderBy}, results are sorted by [\code{recordId}] by default.\cr
#' \cr
#' Valid values for \code{orderBy} are \code{recordId}, \code{massDefect}, \code{molecularWeight}, \code{referenceCount}, \code{dataSourceCount}, \code{pubMedCount}, \code{rscCount}.\cr
#' \cr
#' Valid values for \code{orderDirection} are \code{ascending}, \code{descending}."}\cr
#' \cr
#' If successful, performs the desired query and returns a \code{queryId} for use in \code{chemspiderapi::get_queryId_status()} and \code{chemspideR::get_queryId_results()}.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#' 
#' @param mass A (double) number corresponding to the atomic mass (Da or g/mol) you are inquiring. Has to be within the range of [1,11000].
#' @param range The range for the above mass, also as (double) number. Has to be within the range of [0.0001,100].
#' @param dataSources Optional: A character vector specifying which data source to use. Use \code{chemspiderapi::get_datasources()} for a complete list of data sources. If none are specified (the default), will search all data sources, which can take substantially longer time to complete.
#' @param orderBy A character vector indicating by which parameter to order. Defaults to \code{recordId}; see Details for options.
#' @param orderDirection A character vector indicating in which direction to order; either \code{ascending} (default) or \code{descending}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns a query ID as character vector
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/mass}
#' @examples 
#' \dontrun{
#' ## Post the approximate atomic mass of aspirin and a sensible range
#' mass <- 180
#' range = 0.2
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_mass(mass = mass, range = range, apikey = apikey)
#' }
#' @export
post_mass <- function(mass, range, dataSources = NULL, orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  if (is.na(mass)) {
    warning("No \"mass\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(mass) > 1) {
    warning("This function can only handle a single \"mass\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (is.na(as.double(mass))) {
    warning("The provided \"mass\" is not a valid (double) number; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (mass < 1 || mass > 11000) {
    warning("The provided \"mass\" is outside ChemSpider's settings [1,11000]; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(range) > 1) {
    warning("This function can only handle a single (\"range\") entry; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (is.na(as.double(range))) {
    warning("The provided \"range\" is not a valid (double) number; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (range < 0.0001 || range > 100) {
    warning("The provided \"range\" is outside ChemSpider's settings [0.0001,100]; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (!is.null(dataSources) && length(dataSources) > 20) {
    warning("Only up to 20 different \"dataSources\" are allowed; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(orderBy) > 1) {
    warning("Only a single \"orderBy\" entry is supported; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(orderBy) %in% c("recordid", "massdefect", "molecularweight", "referencecount", "datasourcecount", "pubmedcount", "rsccount")) != 1) {
    warning("Please provide a valid input for \"orderBy\"; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(orderDirection) > 1) {
    warning("Only a single \"orderDirection\" entry is supported; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(tolower(orderDirection) %in% c("ascending", "descending")) != 1) {
    warning("Please use either \"ascending\" or \"descending\" as input for \"orderDirection\"; returning NA.", call. = FALSE)
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
  
  curlData <- list(mass = mass, range = range, orderBy = orderBy, orderDirection = orderDirection)
  
  if (!is.null(dataSources) && length(dataSources) == 1) {
    dataSources <- I(dataSources)
  }
  
  if (!is.null(dataSources)) {
    curlData <- list(mass = mass, range = range, dataSources = dataSources, orderBy = orderBy, orderDirection = orderDirection)
  }
  
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/mass"
  
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
