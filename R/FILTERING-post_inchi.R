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
  
  check_inchi(inchi)
  
  check_apikey(apikey)
  
  data <- list("inchi" = inchi)
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- "https://api.rsc.org/compounds/v1/filter/inchi"
  
  handle <- curl::new_handle()
  curl::handle_setopt(handle, customrequest = "POST", postfields = data)
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  
  result
}
