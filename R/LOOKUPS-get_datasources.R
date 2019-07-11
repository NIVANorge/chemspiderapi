#' @title Get External ChemSpider Data Sources
#' @description Returns a vectorized list of all external ChemSpider data sources.
#' @details If succesfull, returns a character vector listing all available (external) ChemSpider data sources.\cr
#' \cr
#' If not succesfull, returns an error.\cr
#' \cr
#' This function is most useful for narrowing down \code{dataSources} in other chemspiderapi functions, e.g., \code{chemspiderapi::get_references()}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param ... Additional parameters; currently not implemented. 
#' @return A unnamed character vector, length > 350
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/lookups/datasources}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## GET the external data sources of ChemSpider
#' apikey <- "A valid 32-character Chemspider API key"
#' get_datasources(apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON 
#' @export
get_datasources <- function(apikey, ...) {
  
  if (typeof(apikey) != "character") {
    stop("Please provide a 32-character ChemSpider \"apikey\".", call. = FALSE)
  }
  
  if (nchar(apikey) != 32L) {
    stop("Please provide a 32-character ChemSpider \"apikey\".", call. = FALSE)
  }

  curl_header <- list("Content-Type" = "", "apikey" = apikey)
  curl_url <- "https://api.rsc.org/compounds/v1/lookups/datasources"
  curl_handle <- curl::new_handle()
  curl::handle_setopt(curl_handle, customrequest = "GET")
  curl::handle_setheaders(curl_handle, .list = curl_header)
  raw_result <- curl::curl_fetch_memory(url = curl_url, handle = curl_handle)

  if (raw_result$status_code != 200L) {
    
    error_message <- "No ChemSpider Response Error Details were provided."
    
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

    message <- paste0("No valid information was retrieved from the API query.\nCarfully check the validity of the provided ChemSpider \"apikey\".\n", error_message)
    stop(message, call. = FALSE)
  }

  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  result
}
