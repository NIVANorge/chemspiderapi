#' @title POST an InChI string
#' @description Functionality to POST an InChI string to obtain a \code{queryId} for use in \code{chemspiderapi::get_queryID_status()} and \code{chemspiderapi::get_queryId_results()}.
#' @details The validity criteria for InChI strings are outlined here: \url{https://www.inchi-trust.org/technical-faq/#2.8}. If certain criteria are not met by the input \code{inchi}, \code{chemspiderapi::post_inchi()} returns a warning message and \code{NA}, and does not perform an API query.\cr
#' \cr
#' If successful, returns the \code{queryId} as character string.
#' @param inchi A valid InChI string; see Details.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the queryId string as (named) character vector.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/filter/inchi} 
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## POST the InChI string of Aspirin to get a queryId
#' inchi <- "InChI=1S/C9H8O4/c1-6(10)13-8-5-3-2-4-7(8)9(11)12/h2-5H,1H3,(H,11,12)"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' post_inchi(inchi = inchi, apikey = apikey)}
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_inchi <- function(inchi, apikey) {
  
  if (is.null(inchi)) {
    stop("No \"inchi\" string provided.", call. = FALSE)
  }
  
  if (length(inchi) > 1) {
    stop("This function can only handle a single \"inchi\" entry.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  
  if (!grepl(pattern = "InChI=1", x = inchi)) {
    stop("This is not a valid \"inchi\" string because \"InChI=1\" is missing in the beginning.", call. = FALSE)
  }
  
  if (substr(x = inchi, start = 8L, stop = 8L) != "S") {
    warning("This is not a standard \"inchi\" string; performing API query regardless.", call. = FALSE)
  }
  
  if (typeof(apikey) != "character") {
    stop("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
  }
  
  if (nchar(apikey) != 32L) {
    stop("Please use a valid 32 character ChemSpider \"apikey\".", call. = FALSE)
  }
  
  curl_data <- list("inchi" = inchi)
  curl_data <- jsonlite::toJSON(curl_data, auto_unbox = TRUE)
  
  curl_header <- list("Content-Type" = "", "apikey" = apikey)
  
  curl_url <- "https://api.rsc.org/compounds/v1/filter/inchi"
  
  curl_handle <- curl::new_handle()
  curl::handle_setopt(curl_handle, customrequest = "POST", postfields = curl_data)
  
  curl::handle_setheaders(curl_handle, .list = curl_header)
  
  raw_result <- curl::curl_fetch_memory(url = curl_url, handle = curl_handle)
  
  if (raw_result$status_code != 200L) {
    
    error_message <- "No ChemSpider Error Details were provided."
    
    if (raw_result$status_code == 400L) {
      error_message <- "ChemSpider Response Error Details: \"400: Bad Request. Check the request you sent and try again.\"."
    }
    
    if (raw_result$status_code == 401L) {
      error_message <- "ChemSpider Response Error Details: \"401: Unauthorized. Check you have supplied the correct API key and that you have sent it as an HTTP Header called 'apikey'.\"."
    }
    
    if (raw_result$status_code == 404L) {
      error_message <- "ChemSpider Response Error Details: \"404: Not Found. The requested endpoint URL is not recognized. Change your request and try again.\"."
    }
    
    if (raw_result$status_code == 405L) {
      error_message <- "ChemSpider Response Error Details: \"405: Method Not Allowed. The verb is incorrect for the endpoint. Change your request and try again.\"."
    }
    
    if (raw_result$status_code == 413L) {
      error_message <- "ChemSpider Response Error Details: \"413: Payload Too Large. The request you sent was too big to handle. Change your request and try again.\"."
    }
    
    if (raw_result$status_code == 429L) {
      error_message <- "ChemSpider Response Error Details: \"429: Too Many Requests. Send fewer requests, or use rate-limiting to slow them down, then try again.\"."
    }
    
    if (raw_result$status_code == 500L) {
      error_message <- "ChemSpider Response Error Details: \"500: Internal Server Error. Wait and try again.\"."
    }
    
    if (raw_result$status_code == 503L) {
      error_message <- "ChemSpider Response Error Details: \"503: Service Unavailable. Wait and try again.\"."
    }
    
    message <- paste0("No valid results were obtained.\nCarefully check the \"inchi\" string and the validity of the \"apikey\".\n", error_message)
    
    stop(message, call. = FALSE)
  }
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  result
}
