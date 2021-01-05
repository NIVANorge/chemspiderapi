#' @title Get the MOL file of a ChemSpider record
#' @description This function is used to download a single .MOL file from ChemSpider.
#' @details "If successful, returns a SDF file containing the single record for the matching record."\cr
#' \cr 
#' Call this endpoint with \code{recordId} as an integer. To save the MOL/SDF file, see the vignette "Saving MOL Files of Chemicals".
#' @param recordId A valid (integer) ChemSpider ID.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @param simplify \code{logical}: should the results be simplified to a vector? Defaults to \code{FALSE}.
#' @return A character string containing the (human-readable) .MOL file.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/mol} 
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples
#' \dontrun{
#' ## Get the MOL file for caffeine
#' recordId <- 2424L
#' apikey <- "a valid 32-character ChemSpider apikey"
#' get_recordId_mol(recordId = recordId, apikey = apikey)
#' }
#' @importFrom curl curl_fetch_memory handle_setheaders handle_setopt new_handle
#' @importFrom jsonlite fromJSON
#' @export
get_recordId_mol <- function(recordId, apikey, simplify = FALSE) {
  
  .check_recordId(recordId)
  
  .check_apikey(apikey)
  
  .check_simplify(simplify)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  base_url <- Sys.getenv("GET_RECORDID_URL",
                         "https://api.rsc.org/compounds/v1/records/")
  
  url <- paste0(base_url, recordId, "/mol")
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "GET")
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  .check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  
  if (simplify) {
    result <- unlist(result, use.names = FALSE)
  }
  
  result
}
