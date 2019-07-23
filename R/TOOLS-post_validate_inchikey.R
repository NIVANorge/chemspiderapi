#' @title Validate an InChIKey
#' @description Functionality to check the validity of an InChIKey.
#' @details If successful (i.e., the InChIKey is valid), returns \code{TRUE}.\cr
#' \cr
#' If not successful (i.e., the InChIKey is not valid), returns \code{FALSE}.\cr
#' \cr
#' Before this functions performs an API query, it runs quality checking as lined out at \url{https://www.inchi-trust.org/technical-faq/#13.1}. If an InChIKey is ruled out based on these criteria, it returns \code{FALSE} with a warning message.
#' @param inchikey A 27-character InChIKey to be validated.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A logical indicating the validity of the POSTed InChIKey
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/post/tools/validate/inchikey}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## validate the InChIKey of aspirin
#' inchikey <- "BSYNRYMUTXBXSQ-UHFFFAOYSA-N"
#' apikey <- "a_valid_ChemSpider_API_key"
#' post_validate_inchikey(inchikey = inchikey, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON toJSON
#' @export
post_validate_inchikey <- function(inchikey, apikey) {
  
  check_inchikey(inchikey)
  
  check_apikey(apikey)
  
  data <- list("inchikey" = inchikey)
  
  data <- jsonlite::toJSON(data, auto_unbox = TRUE)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- "https://api.rsc.org/compounds/v1/tools/validate/inchikey"
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "POST", postfields = data)
  
  curl::handle_setheaders(handle, .list = header)
  
  result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  if (result$status_code == 200) {
    TRUE
  } else {
    FALSE
  }
}
