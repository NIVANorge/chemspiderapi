#' @title Get External ChemSpider Data Sources
#' @description Returns a vectorized list of all external ChemSpider data sources.
#' @details If succesfull, returns a character vector listing all available (external) ChemSpider data sources.\cr
#' \cr
#' If not succesfull, returns an error.\cr
#' \cr
#' This function is most useful for narrowing down \code{dataSources} in other chemspiderapi functions, e.g., \code{chemspiderapi::get_references()}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
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
get_datasources <- function(apikey) {
  
  check_apikey(apikey)

  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- "https://api.rsc.org/compounds/v1/lookups/datasources"
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "GET")
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)

  check_status_code(raw_result$status_code)

  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  result
}
