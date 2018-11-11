#' GET external references of a record ID
#' 
#' GET a list of external references for a ChemSpider ID.
#' 
#' It is recommended to specify which \code{dataSources} to load, as some substances have a substantial amount of references (>10'000). Use \code{chemspiderapi::get_datasources()} for a complete list of the >300 \code{dataSources}.\cr
#' \cr
#' If successful, returns a data frame with four columns: \code{source}, \code{sourceUrl}, \code{externalId}, and \code{externalUrl}; unless any of them are removed by setting them to \code{FALSE} in the function call. In case only one parameter, e.g. the external ID, is returned, the result is a vector.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
#' 
#' @param recordId A valid (integer) ChemSpider ID.
#' @param dataSources Either a character string, a vector of characters, or a list of characters detailing which \code{dataSources} to look up. If left as is, will return all \code{dataSources}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param source \code{logical}: Should the source name be returned (ChemSpider default)?
#' @param sourceUrl \code{logical}: Should the source URL be returned (ChemSpider default)?
#' @param externalUrl \code{logical}: Should the external URL be returned (ChemSpider default)?
#' @return Either a data frame (if no options are dropped) or a character/integer vector, depending on the external ID.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/externalreferences}
#' @examples 
#' \dontrun{
#' ## GET the PubChem external reference for aspirin
#' recordId <- 2157L
#' dataSources <- "PubChem"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' get_recordId_externalreferences(recordId = recordId, dataSources = dataSources, apikey = apikey)
#' }
#' @export 
get_recordId_externalreferences <- function(recordId, dataSources = NULL, apikey, source = TRUE, sourceUrl = TRUE, externalUrl = TRUE) {
  if (is.na(recordId)) {
    warning("No \"recordId\" provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(recordId) > 1) {
    warning("This function can only handle a single \"recordId\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (is.na(as.integer(recordId))) {
    warning("Please use a valid \"recordId\"; returning NA.", call. = FALSE)
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
  
  if (!is.null(dataSources)) {
    dataSources <- paste(dataSources, collapse = ",")
  }
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  if (!is.null(dataSources)) {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/externalreferences?dataSources=", dataSources)
  }
  
  else {
    curlUrl <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/externalreferences")
  }
  
  curlHandle <- curl::new_handle()
  
  curl::handle_setopt(curlHandle, customrequest = "GET")
  
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
  
  if (source == FALSE) {
    result$source <- NULL
  }
  
  if (sourceUrl == FALSE) {
    result$sourceUrl <- NULL
  }
  
  if (externalUrl == FALSE) {
    result$externalUrl <- NULL
  }
  
  if (ncol(result) == 1) {
    result <- unlist(result)
  }
  
  return(result)
}
