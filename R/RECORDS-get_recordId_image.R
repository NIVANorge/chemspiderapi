#' @title GET a PNG image of a ChemSpider record ID
#' @description This function is used to obtain a .png image file of a ChemSpider record ID, e.g., after \code{chemspiderapi::get_queryID_results()}.
#' @details If succesfull, returns either a base64-decoded character vector (\code{png = TRUE}) or a numeric (double) array (\code{png = TRUE}). Either of them can be written to the hard drive using \code{chemspiderapi::write_image()}.
#' @param recordId A valid (integer) ChemSpider ID.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param png \code{logical}: Should the base64-encoded character string be converted to a (raw) .png image?
#' @return Either a base64-encoded character string (\code{png = FALSE}) or a raw array (\code{png = TRUE}).
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/image} 
#' @examples \dontrun{
#' ## GET the .png image for aspirin
#' recordId <- 2157L
#' apikey <- "a_valid_ChemSpider_API_key"
#' get_recordId_image(recordId = recordId, apikey = apikey)
#' get_recordId_image(recordId = recordId, apikey = apikey, png = TRUE)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite base64_dec fromJSON toJSON
#' @importFrom png readPNG
#' @export 
get_recordId_image <- function(recordId, apikey, png = FALSE) {
  
  if (is.null(recordId)) {
    stop("No \"recordId\" provided.", call. = FALSE)
  }
  
  if (length(recordId) > 1) {
    stop("This function can only handle a single \"recordId\" entry.\nFor functional programming, try using it in apply() or purrr::map().", call. = FALSE)
  }
  
  if (is.na(as.integer(recordId))) {
    stop("Please provide a valid (integer) \"recordId\".", call. = FALSE)
  }
  
  if (typeof(apikey) != "character") {
    stop("The ChemSpider \"apikey\" should be a 32-character string.", call. = FALSE)
  }
  
  if (nchar(apikey) != 32L) {
    stop("Please use a valid 32-character ChemSpider \"apikey\".", call. = FALSE)
  }
  
  if (!is.logical(png)) {
    stop("The \"png\" argument must be either TRUE or FALSE.", call. = FALSE)
  }
  
  curl_header <- list("Content-Type" = "", "apikey" = apikey)
  
  curl_url <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/image")
  
  curl_handle <- curl::new_handle()
  
  curl::handle_setopt(curl_handle, customrequest = "GET")
  
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
    
    if (raw_result$status_code == 429L) {
      error_message <- "ChemSpider Response Error Details: \"429: Too Many Requests. Send fewer requests, or use rate-limiting to slow them down, then try again.\"."
    }
    
    if (raw_result$status_code == 500L) {
      error_message <- "ChemSpider Response Error Details: \"500: Internal Server Error. Wait and try again.\"."
    }
    
    if (raw_result$status_code == 503L) {
      error_message <- "ChemSpider Response Error Details: \"503: Service Unavailable. Wait and try again.\"."
    }
    
    message <- paste0("No valid results were obtained; returning \"NA\".\nCarefully check the \"recordId\" and the validity of the \"apikey\".\n", error_message)
    
    stop(message, call. = FALSE)
  }
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  if (png) {
    result <- jsonlite::base64_dec(result)
    result <- png::readPNG(result)
  }
  
  result
}
