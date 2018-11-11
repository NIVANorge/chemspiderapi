#' POST an InChI string
#' 
#' Functionality to POST an InChI string to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryID_status()} and \code{chemspiderapi::get_queryId_results()}.
#' 
#' The validity criteria for InChI strings are outlined here: \url{https://www.inchi-trust.org/technical-faq/#2.8}. If certain criteria are not met by the input \code{inchi}, \code{chemspiderapi::post_inchi()} returns a warning message and \code{NA}, and does not perform an API query.\cr
#' \cr
#' If successful, returns the \code{queryId} as character string.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#' 
#' @param inchi A valid InChI string; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Return a query ID as character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/inchi} 
#' @examples 
#' \dontrun{
#' ## POST the InChI string of Aspirin to get a queryId
#' inchi <- "InChI=1S/C9H8O4/c1-6(10)13-8-5-3-2-4-7(8)9(11)12/h2-5H,1H3,(H,11,12)"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_inchi(inchi = inchi, apikey = apikey)}
#' @export
post_inchi <- function(inchi, apikey) {
  
  if (is.na(inchi)) {
    warning("No valid \"inchi\" string provided; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (length(inchi) > 1) {
    warning("This function can only handle a single \"inchi\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (substr(tolower(inchi), start = 1L, stop = 7L) != "inchi=1") {
    warning("This is not a valid \"inchi\" string because \"InChI=1\" is missing in the beginning; returning \"NA\".", call. = FALSE)
    return(NA_character_)
  }
  
  if (substr(tolower(inchi), start = 8L, stop = 8L) != "s") {
    warning("This is not a standard \"inchi\" string; performing API query regardless.", call. = FALSE)
  }
  
  if (length(apikey) > 1L) {
    warning("This function can only handle a single \"apikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA_character_)
  }
  
  if (typeof(apikey) != "character") {
    warning("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
    return(NA_character_)
  }
  
  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32 character ChemSpider \"apikey\".", call. = FALSE)
    return(NA_character_)
  }
  
  curlData <- list(inchi = inchi)
  curlData <- jsonlite::toJSON(curlData, auto_unbox = TRUE)
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- "https://api.rsc.org/compounds/v1/filter/inchi"
  
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
    
    if (result$status_code == 413L) {
      error_message <- "\nChemSpider Response Error Details: \"413: Payload Too Large. The request you sent was too big to handle. Change your request and try again.\"."
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
