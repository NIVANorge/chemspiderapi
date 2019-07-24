#' @title Get results of a mass batch query from ChemSpider
#' @description Get results from a ChemSpider query after \code{chemspiderapi::get_mass_batch_queryId_status()} returns \code{"Complete"}.
#' @details Before running \code{chemspiderapi::get_mass_batch_queryId_results()}, make sure \code{chemspiderapi::get_mass_batch_queryIdstatus()} returns \code{"Complete"}.
#' @param queryId A valid 36-character query ID, as returned by \code{chemspiderapi::post_mass_batch()}.
#' @param status status A character string indicating the query status as returned by \code{chemspiderapi::get_mass_batch_queryId_status()}
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns the record IDs of a mass batch query 
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/filter/mass/batch/{queryId}/results}
#' @author Raoul Wolf (\url{https://github.com/RaoulWolf/})
#' @examples 
#' \dontrun{
#' ## Obtain the result from a mass batch query
#' apikey <- "a valid 32-character ChemSpider apikey"
#' queryId <- "a valid 36-character ChemSpider queryId"
#' get_mass_batch_queryId_results(queryId = queryId, apikey = apikey)
#' }
#' @export
get_mass_batch_queryId_results <- function(queryId, status, apikey) {
  
  check_queryId(queryId)
  
  check_status(status)
  
  check_apikey(apikey)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- paste0("https://api.rsc.org/compounds/v1/filter/mass/batch/", queryId, "/results")
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "GET")
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- as.data.frame(results = result$results, stringsAsFactors = FALSE)
  
  check_result(result)
  
  result
}
