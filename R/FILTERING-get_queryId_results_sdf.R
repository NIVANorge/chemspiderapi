#' GET the gzipped .sdf file for a ChemSpider query
#' 
#' This function is used to download a single .sdf file from ChemSpider after \code{chemspiderapi::get_queryId_status()} returns \code{"Complete"}.
#' 
#' Call this function after \code{chemspiderapi::get_queryId_status()} returns \code{"Complete"}.\cr
#' \cr
#' If successful, returns a character vector of results from the query, a gzipped .sdf file.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#' 
#' @param queryId A valid 36-character ChemSpider \code{queryId}.
#' @param status A character string indicating the query status as returned by \code{chemspiderapi::get_queryId_status()}.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return Returns a (base64-encoded) character vector
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/filter/{queryId}/results/sdf}
#' @examples 
#' \dontrun{
#' ## GET a gzipped .sdf file
#' queryId <- "a valid 36-character ChemSpider apikey"
#' apikey <- "a valid 32-character ChemSpider apikey"
#' get_queryId_results_sdf(queryId = queryId, apikey = apikey)
#' }
#' @export
get_queryId_results_sdf <- function(queryId, status, apikey) {
  
  check_queryId(queryId)
  
  check_status(status)
  
  check_apikey(apikey)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- paste0("https://api.rsc.org/compounds/v1/filter/", queryId, "/results/sdf")
  
  handle <- curl::new_handle()
  
  curl::handle_setopt(handle, customrequest = "GET")
  
  curl::handle_setheaders(handle, .list = header)
  
  raw_result <- curl::curl_fetch_memory(url = url, handle = handle)
  
  check_status_code(raw_result$status_code)
  
  result <- rawToChar(raw_result$content)
  result <- jsonlite::fromJSON(result)
  result <- unlist(result$results)
  
  result
}
