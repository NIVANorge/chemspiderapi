#' @title Get a PNG image of a ChemSpider record
#' @description This function is used to obtain a 250 x 250 pixel PNG image file of a ChemSpider record ID, e.g., after \code{chemspiderapi::get_queryID_results()}.
#' @details Returns a numeric (double) array. To save the picture, see the vignette "Saving PNG Images of Chemicals".
#' @param recordId A valid (integer) ChemSpider ID.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A numeric array.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/image} 
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples \dontrun{
#' ## Get the PNG image for caffeine
#' recordId <- 2424L
#' apikey <- "a_valid_ChemSpider_API_key"
#' get_recordId_image(recordId = recordId, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite base64_dec fromJSON
#' @export 
get_recordId_image <- function(recordId, apikey) {
  
  check_recordId(recordId)
  
  check_apikey(apikey)

  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/image")
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "GET")
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result)
  result <- jsonlite::base64_dec(result)

  result
}
