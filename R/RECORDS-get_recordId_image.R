#' GET a .png image of a ChemSpider record ID
#' 
#' This function is used to obtain a .png image file of a ChemSpider record ID, e.g., after \code{chemspiderapi::get_queryID_results()}.
#' 
#' If succesfull, returns either a base64-decoded character vector (\code{png = TRUE}) or a numeric (double) array (\code{png = TRUE}). Either of them can be written to the hard drive using \code{chemspiderapi::write_image()}.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map()}.
#' 
#' @param recordId A valid (integer) ChemSpider ID.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param png \code{logical}: Should the base64-encoded character string be converted to a (raw) .png image?
#' @return Either a base64-encoded character string (\code{png == FALSE}) or a raw array (\code{png == TRUE}).
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/image} 
#' @examples
#' \dontrun{
#' ## GET the .png image for aspirin
#' recordId <- 2157L
#' apikey <- "a_valid_ChemSpider_API_key"
#' get_recordId_image(recordId = recordId, apikey = apikey)
#' get_recordId_image(recordId = recordId, apikey = apikey, png = TRUE)
#' }
#' @export 
get_recordId_image <- function(recordId, apikey, png = FALSE) {
  if (is.na(recordId)) {
    warning("No \"recordId\" provided; returning \"NA\".", call. = FALSE)
    return(NA)
  }
  
  if (length(recordId) > 1) {
    warning("This function can only handle a single \"recordId\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA)
  }
  
  if (is.na(as.integer(recordId))) {
    warning("Please use a valid (integer) \"recordId\"; returning \"NA\".", call. = FALSE)
    return(NA)
  }
  
  if (length(apikey) > 1L) {
    warning("This function can only handle a single ChemSpider \"apikey\" entry; returning \"NA\".\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
    return(NA)
  }
  
  if (typeof(apikey) != "character") {
    warning("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
    return(NA)
  }
  
  if (nchar(apikey) != 32L) {
    warning("Please use a valid 32-character ChemSpider \"apikey\".", call. = FALSE)
    return(NA)
  }
  
  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  
  curlUrl <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/image")
  
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
    return(NA)
  }
  
  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  if (png == TRUE) {
    result <- jsonlite::base64_dec(result)
    result <- png::readPNG(result)
  }
  
  return(result)
}
