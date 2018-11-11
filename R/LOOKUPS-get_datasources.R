#' GET a all external ChemSpider data sources
#'
#' Returns a listing of all external ChemSpider data sources.
#'
#' If succesfull, return a character vector listing all available (external) data sources.\cr
#' \cr
#' If not succesfull, returns a warning and \code{NA}.\cr
#' \cr
#' This function is most useful for narrowing down \code{dataSources} in other \code{chemspiderapi} functions, e.g., \code{chemspiderapi::get_references()}.
#'
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A unnamed character vector, length > 350
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/lookups/datasources}
#' @examples
#' \dontrun{
#' ## GET the external data sources of ChemSpider
#' apikey <- "A valid 32-character Chemspider API key"
#' get_datasources(apikey = apikey)
#' }
#' @export
get_datasources <- function(apikey) {

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

  curlHeader <- list(`Content-Type` = "", apikey = apikey)

  curlUrl <- "https://api.rsc.org/compounds/v1/lookups/datasources"

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

    message <- paste0("No valid information was retrieved; returning \"NA\".\nCarfully check the validity of the \"apikey\".", error_message)

    warning(message, call. = FALSE)
    return(NA_character_)
  }

  result <- rawToChar(result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  result <- unname(result)

  return(result)
}
