#' POST a batch of monoisotopic masses and their ranges to obtain a query ID
#' 
#' Functionality to POST up to 20 monoisotopic masses and their range to obtain a \code{queryId} for use in \code{chemspiderapi::get_mass_batch_queryId_status()} and \code{chemspiderapi::get_mass_batch_queryId_results()}.
#' 
#' Says Chemspider:\cr
#' \cr
#' \emph{"If \code{dataSources} is not specified, all known sources are searched. This will take longer.\cr
#' \cr
#' Optionally, you can also submit \code{orderBy} and \code{orderDirection} to specify the sort order for the results. If you do not specify a value for \code{orderBy}, results are sorted by \code{recordId} by default.\cr
#' \cr
#' Valid values for \code{orderBy} are \code{recordId}, \code{massDefect}, \code{molecularWeight}, \code{referenceCount}, \code{dataSourceCount}, \code{pubMedCount}, \code{rscCount}.\cr
#' \cr
#' Valid values for \code{orderDirection} are \code{ascending}, \code{descending}."}\cr
#' \cr
#' If successful, performs the desired query and returns a \code{queryId} for use in \code{chemspider::get_mass_batch_queryId_status()} and \code{chemspideR::get_mass_batch_queryId_results()}.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#' 
#' @param mass A vector of (double) numbers corresponding to the atomic (monoisotopic) masses you are inquiring. Has to be within the range of [1,11000].
#' @param range A vector of (double) numbers corresponding to the rangees for the above masses. Has to be within the range of [0.0001,0.001].
#' @param dataSources Optional: A character vector specifying which data source to use. Use \code{chemspiderapi::get_datasources()} for a complete list of data sources. If none are specified (the default), will search all data sources, which can take substantially longer time to complete.
#' @param orderBy A character vector indicating by which parameter to order. Defaults to \code{recordId}; see Details for options.
#' @param orderDirection A character vector indicating in which direction to order; either \code{ascending} (default) or \code{descending}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns a query ID as character vector
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/mass/batch}
#' @examples 
#' \dontrun{
#' ## POST two atomic (monoisotopic) masses and their ranges
#' mass <- c(180, 246)
#' range <- c(0.0005, 0.001)
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_mass_batch(mass = mass, range = range, apikey = apikey)
#' }
#' @export
post_mass_batch <- function(mass, range, dataSources = NULL, orderBy = "recordId", orderDirection = "ascending", apikey) {
  
  if (is.na(mass)) {
    warning("No \"mass\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (is.na(range)) {
    warning("No \"mass\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(mass) == 1) {
    warning("This function is meant for multiple \"mass\" entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(mass) > 20) {
    warning("This function is only meant for up to 20 \"mass\" entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(is.na(as.double(mass))) > 0) {
    warning("The provided \"mass\" is not a valid (double) number; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (any(mass < 1 || mass > 11000)) {
    warning("The provided \"mass\" is outside ChemSpider's settings [1,11000]; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(range) == 1) {
    warning("This function is meant for multiple \"range\" entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(range) > 20) {
    warning("This function is only meant for up to 20 \"range\" entries; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (sum(is.na(as.double(range))) > 0) {
    warning("The provided \"range\" is not a valid (double) number; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (any(range < 0.0001 || range > 0.001)) {
    warning("The provided \"range\" is outside ChemSpider's settings [0.0001,0.001]; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(mass) != length(range)) {
    warning("Every \"mass\" needs a \"range\", and vice verca; returning \"NA\".", call. = FALSE)
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
  
  if (!is.null(dataSources) && length(dataSources) > 20) {
    warning("Only up to 20 different \"dataSources\" are allowed, please narrow it down; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  masses <- list()
  
  for (i in 1:length(mass)) {
    masses[[i]] <- list(mass = mass[i], range = range[i])
  }
  
  curlData <- list(masses = masses, orderBy = orderBy, orderDirection = orderDirection)
  
  if (!is.null(dataSources) && length(dataSources) == 1) {
      dataSources <- I(dataSources)
  }
  
  if (!is.null(dataSources)) {
    curlData <- list(masses = masses, dataSources = dataSources, orderBy = orderBy, orderDirection = orderDirection)
  }
  
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/mass/batch"
  
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
