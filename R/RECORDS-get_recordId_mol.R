#' GET the .MOL of a ChemSpider record
#' 
#' This function is used to download a single .MOL file from ChemSpider.
#' 
#' Call this endpoint with \code{recordId} as an integer.\cr
#' \cr
#' If successful, returns a character vector of result of the query, i.e., a .MOL file. This character vector can be written to the hard drive using \code{chemspiderapi::write_mol()}.\cr
#' \cr
#' If not successful, returns \code{NA}.\cr
#' \cr
#' This function is fully \code{tidyverse} compatible, e.g., for use in \code{purrr::map_chr()}.
#'  
#' @param recordId A valid (integer) ChemSpider ID.
#' @param apikey A 32-character string with a valid key for ChemSpider's API services.
#' @return A character string containing the (human-readable) .MOL file.
#' @seealso \url{https://developer.rsc.org/compounds-v1/apis/get/records/{recordId}/mol} 
#' @examples
#' \dontrun{
#' ## GET the .MOL file for aspirin
#' recordId <- 2157L
#' apikey <- "a valid 32-character ChemSpider apikey"
#' get_recordId_mol(recordId = recordId, apikey = apikey)
#' }
#' @export
get_recordId_mol <- function(recordId, apikey) {
  
  check_recordId(recordId)
  
  check_apikey(apikey)
  
  header <- list("Content-Type" = "", "apikey" = apikey)
  
  url <- paste0("https://api.rsc.org/compounds/v1/records/", recordId, "/mol")
  
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
