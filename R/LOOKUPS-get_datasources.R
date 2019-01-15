#' Get External ChemSpider Data Sources
#'
#' Returns a vectorized list of all external ChemSpider data sources.
#'
#' If succesfull, returns a character vector listing all available (external) ChemSpider data sources.\cr
#' \cr
#' If not succesfull, returns an error.\cr
#' \cr
#' This function is most useful for narrowing down \code{dataSources} in other chemspiderapi functions, e.g., \code{chemspiderapi::get_references()}.
#'
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param ... Additional parameters; currently not implemented. 
#' @return A unnamed character vector, length > 350
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/lookups/datasources}
#' @examples
#' \dontrun{
#' ## GET the external data sources of ChemSpider
#' apikey <- "A valid 32-character Chemspider API key"
#' get_datasources(apikey = apikey)
#' }
#' @export
get_datasources <- function(apikey, ...) {
  
  if (length(apikey) > 1L) {
    stop("Please provide a single ChemSpider \"apikey\".", call. = FALSE)
  }
  
  if (sum(typeof(apikey) %in% c("character", "factor") < 1L)) {
    stop("Please provide a 32-character ChemSpider \"apikey\".", call. = FALSE)
  }
  
  if (is.factor(apikey)) {
    apikey <- as.character(apikey)
  }

  if (nchar(apikey) != 32L) {
    stop("Please provide a 32-character ChemSpider \"apikey\".", call. = FALSE)
  }

  curlHeader <- list(`Content-Type` = "", apikey = apikey)
  curlUrl <- "https://api.rsc.org/compounds/v1/lookups/datasources"
  curlHandle <- curl::new_handle()
  curl::handle_setopt(curlHandle, customrequest = "GET")
  curl::handle_setheaders(curlHandle, .list = curlHeader)
  result <- curl::curl_fetch_memory(url = curlUrl, handle = curlHandle)

  if (result$status_code != 200L) {
    
    error_message <- "\nNo ChemSpider Response Error Details were provided."
    
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

    message <- paste0("No valid information was retrieved.\nCarfully check the validity of the provided ChemSpider \"apikey\".", error_message)
    stop(message, call. = FALSE)
  }

  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  result <- unname(result)
  result
}
